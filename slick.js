//@+leo-ver=5-thin
//@+node:gcross.20110626200911.1121: * @file slick.js
//@@language javascript

//@+<< Prelude >>
//@+node:gcross.20110626200911.1127: ** << Prelude >>
"use strict"
//@-<< Prelude >>

//@+<< Global variables >>
//@+node:gcross.20110627234551.1181: ** << Global variables >>
var svg_namespace = "http://www.w3.org/2000/svg"
var xlink_namespace = "http://www.w3.org/1999/xlink"

// Keycodes.
var LEFT_KEY = 37; // cursor left keycode
var UP_KEY = 38; // cursor up keycode
var RIGHT_KEY = 39; // cursor right keycode
var DOWN_KEY = 40; // cursor down keycode
var PAGE_UP_KEY = 33; // page up keycode
var PAGE_DOWN_KEY = 34; // page down keycode
var HOME_KEY = 36; // home keycode
var END_KEY = 35; // end keycode
var ENTER_KEY = 13; // next slide
var SPACE_KEY = 32;
var ESCAPE_KEY = 27;
var F5_KEY = 116;

var director
//@-<< Global variables >>

//@+others
//@+node:gcross.20110627234551.1201: ** Initialization
//@+node:gcross.20110627234551.1180: *3* function initializeSlick
function initializeSlick(script) {
    director = new Director(script)
    
    window.addEventListener("keydown",function(event) {
        switch(event.keyCode || event.charCode) {
            case SPACE_KEY:
            case ENTER_KEY:
            case F5_KEY:
                if(director.animator) {
                    var animator = director.animator
                    if(animator.active())
                        animator.pause()
                    else
                        animator.start()
                } else {
                    director.playSlide()
                }
                break
            case LEFT_KEY:
            case PAGE_UP_KEY:
                director.rewind()
                break
            case RIGHT_KEY:
            case PAGE_DOWN_KEY:
                director.fastforward()
                break
            case UP_KEY:
                director.rewind(10)
                break
            case DOWN_KEY:
                director.fastforward(10)
                break
        }
    },false)
    
    if(window.location.hash) {
        director.gotoSlide(window.location.hash.substring(1))
    } else {
        director.playUntilTagReached()
    }

    window.addEventListener("hashchange",function(event) {
        if(window.location.hash) director.gotoSlide(window.location.hash.substring(1))
    },false)

    document.documentElement.appendChild(director.stage.getNode())
}
//@+node:gcross.20110712230720.1200: ** Values
var default_value = undefined
//@+node:gcross.20110702143209.1186: ** Functions
//@+node:gcross.20110702143209.1187: *3* convertStringToGetter
function convertStringToGetter(getObjectFromStage) {
    if(typeof getObjectFromStage == "string")
        return function(stage) { return stage[getObjectFromStage]; }
    else
        return getObjectFromStage
}
//@+node:gcross.20110712230720.1197: *3* styleFor
function styleFor(selector) {
    return function(stage) {
        return stage.lookupStyleFor(selector)
    }
}
//@+node:gcross.20110712230720.1199: *3* styleOf
function styleOf(actor_name) {
    return function(stage) {
        return stage[actor_name].style
    }
}
//@+node:gcross.20110626200911.1122: ** class Stage
function Stage() {
    this.nodes = {}
    this.ordering = []
    this.styles = {}
    var style_node = document.createElementNS(svg_namespace,"style")
    style_node.setAttribute("title","slick_stylesheet")
    document.documentElement.appendChild(style_node)
    this.stylesheet = (function() {
        for(var i = 0; i < document.styleSheets.length; ++i) {
            if(document.styleSheets[i].title == "slick_stylesheet") {
                return document.styleSheets[i]
            }
        }
        throw Error('Unable to find newly created style sheet "slick_stylesheet"')
    })()
}
Stage.prototype = {
    //@+others
    //@+node:gcross.20110626200911.1134: *3* addActorNode
    addActorNode: function(name,actor) {
        var node = actor.getNode()
        this.nodes[name] = node
        return node
    },
    //@+node:gcross.20110626200911.1124: *3* appendActor
    appendActor: function(name,actor) {
        this.assertActorNotPresent(name)
        this[name] = actor
        this.ordering.push(name)
        if(this.node) this.node.appendChild(this.addActorNode(name,actor))
    },
    //@+node:gcross.20110702143209.1197: *3* assertActorNotPresent
    assertActorNotPresent: function(name) {
        if(name in this) throw new Error(name + " has already been placed on the stage!")
    },
    //@+node:gcross.20110702143209.1195: *3* assertActorPresent
    assertActorPresent: function(name) {
        if(!(name in this)) throw new Error(name + " is not present on the stage!")
    },
    //@+node:gcross.20110627234551.1169: *3* getActor
    getActor: function(name) { return this[name]; },
    //@+node:gcross.20110627234551.1165: *3* getActorNameAfter
    getActorNameAfter: function(name,excluding) {
        var index = this.ordering.indexOf(name)
        ++index
        if(excluding) {
            while(index < this.ordering.length && this.ordering[index] in excluding) ++index;
        }
        if(index < this.ordering.length)
            return this.ordering[index]
        else
            return null
    },
    //@+node:gcross.20110627234551.1159: *3* getNode
    getNode: function() {
        this.prepareNode()
        return this.node
    },
    //@+node:gcross.20110626200911.1125: *3* insertActorBefore
    insertActorBefore: function(name,actor,before_name) {
        this.assertActorNotPresent(name)
        if(before_name == undefined) {
            this.appendActor(name,actor)
            return
        }
        this[name] = actor
        this.ordering.splice(this.ordering.indexOf(before_name),0,name)
        if(this.node) this.node.insertBefore(this.addActorNode(name,actor),this.nodes[before_name])
    },
    //@+node:gcross.20110711171503.1274: *3* lookupStyleFor
    lookupStyleFor: function(selector) {
        var style = this.styles[selector]
        if(style == undefined) {
            style = this.stylesheet.cssRules[this.stylesheet.insertRule(selector + " {}")].style
            this.styles[selector] = style
        }
        return style
    },
    //@+node:gcross.20110702143209.1193: *3* moveActorBefore
    moveActorBefore: function(name,name_before) {
        if(!name_before) return this.moveActorToEnd(name)
        this.assertActorPresent(name)
        this.assertActorPresent(name_before)
        this.ordering.splice(this.ordering.indexOf(name),1)
        this.ordering.splice(this.ordering.indexOf(name_before),0,name)
        if(this.node) {
            var node = this[name].node
            this.node.removeChild(node)
            this.node.insertBefore(node,this[name_before].node)
        }
    },
    //@+node:gcross.20110702143209.1191: *3* moveActorToEnd
    moveActorToEnd: function(name) {
        this.assertActorPresent(name)
        this.ordering.splice(this.ordering.indexOf(name),1)
        this.ordering.push(name)
        if(this.node) {
            var node = this[name].node
            this.node.removeChild(node)
            this.node.appendChild(node)
        }
    },
    //@+node:gcross.20110629133112.1177: *3* prepareNode
    prepareNode: function() {
        if(this.node == undefined) {
            this.node = document.createElementNS(svg_namespace,"g")
            var self = this
            this.ordering.forEach(function(name) {
                var node = self[name].getNode()
                self.nodes[name] = node
                self.node.appendChild(node)
            })
        }
    },
    //@+node:gcross.20110626200911.1129: *3* removeActor
    removeActor: function(name) {
        if(!(name in this)) throw new Error(name + " is not present on the stage to be fired!")
        var actor = this[name]
        delete this[name]
        if(this.node) this.node.removeChild(actor.node)
        this.ordering.splice(this.ordering.indexOf(name),1)
        return actor
    },
    //@+node:gcross.20110626200911.1133: *3* update
    update: function() {
        this.prepareNode()
        this.ordering.forEach(function(name) { this[name].update(); },this)
    }
    //@-others
}
//@+node:gcross.20110627234551.1189: ** class Animator
function Animator(director,animation,callback) {
    this.director = director
    this.animation = animation
    this.callback = callback
}
Animator.prototype = {
    //@+<< Initial field values >>
    //@+node:gcross.20110627234551.1191: *3* << Initial field values >>
    offset: 0,
    interval: 1/100,
    //@-<< Initial field values >>
    //@+others
    //@+node:gcross.20110627234551.1198: *3* active
    active: function() { return ("handler_id" in this); },
    //@+node:gcross.20110627234551.1195: *3* disable
    disable: function() {
        window.clearInterval(this.handler_id)
        delete this.handler_id
        delete this.starting_time
    },
    //@+node:gcross.20110627234551.1194: *3* pause
    pause: function() {
        this.offset = new Date() - this.starting_time
        this.disable()
    },
    //@+node:gcross.20110627234551.1190: *3* start
    start: function() {
        this.starting_time = new Date() - this.offset
        delete this.offset
        var self = this;
        this.handler_id = window.setInterval(function() { self.step(); },this.interval*1000)
    },
    //@+node:gcross.20110627234551.1192: *3* step
    step: function() {
        var animation = this.animation
        var current_time = (new Date() - this.starting_time) / 1000
        if(current_time < animation.duration) {
            var stage = this.director.stage
            animation.stepTo(stage,current_time)
            stage.update()
        } else {
            this.stop()
            delete this.director.animator
            this.director.advance()
            this.director.update()
            this.callback()
        }
    },
    //@+node:gcross.20110627234551.1193: *3* stop
    stop: function() {
        delete this.offset
        this.disable()
    }
    //@-others
}
//@+node:gcross.20110627234551.1148: ** class Director
function Director(script) {
    //@+<< Initialization >>
    //@+node:gcross.20110627234551.1150: *3* << Initialization >>
    this.tags = {"0":0}
    this.script_slides = []
    this.slides = [0]
    var slide = 0
    var tag_chunk_in_progress = false
    for(var index = 0; index < script.length; ++index) {
        if(typeof script[index] == "string") {
            if(script[index]) this.tags[script[index]] = index
            if(!tag_chunk_in_progress) {
                ++slide
                this.slides.push(index)
                this.tags[String(slide)] = index
                tag_chunk_in_progress = true
            }
        } else {
            tag_chunk_in_progress = false
        }
        this.script_slides[index] = slide
    }
    ++slide
    this.slides.push(index)
    this.tags[String(slide)] = index
    this.script_slides.push(slide)

    this.script = script
    this.stage = new Stage
    //@-<< Initialization >>
}
Director.prototype = {
    //@+<< Initial field values >>
    //@+node:gcross.20110627234551.1171: *3* << Initial field values >>
    marker: 0,
    prepared: 0,
    play_in_progress: false,
    //@-<< Initial field values >>
    //@+others
    //@+node:gcross.20110627234551.1172: *3* advance
    advance: function() {
        this.prepare()
        if(!this.atTag()) this.script[this.marker].advance(this.stage)
        ++this.marker
    },
    //@+node:gcross.20110627234551.1188: *3* atTag
    atTag: function() { return (typeof this.script[this.marker] == "string"); },
    //@+node:gcross.20110627234551.1179: *3* fastforward
    fastforward: function(n) {
        if(n == undefined) n = 1
        if(this.animator) {
            this.animator.stop()
            delete this.animator
            this.script[this.marker].advance(this.stage)
            ++this.marker
            n -= this.script_slides[this.marker] - this.script_slides[this.marker-1]
        }
        this.gotoSlide(this.getCurrentSlide()+n)
    },
    //@+node:gcross.20110629133112.1179: *3* getCurrentSlide
    getCurrentSlide: function() { return this.script_slides[this.marker]; },
    //@+node:gcross.20110627234551.1175: *3* gotoIndex
    gotoIndex: function(index) {
        while(this.marker > index) {
            this.retract()
        }
        while(this.marker < index) {
            this.advance()
        }
    },
    //@+node:gcross.20110627234551.1176: *3* gotoSlide
    gotoSlide: function(destination) {
        switch(typeof destination) {
            case 'number':
                destination = Math.max(1,Math.min(this.slides.length-1,destination))
                this.gotoIndex(this.slides[destination])
                break
            case 'string':
                this.gotoIndex(this.tags[destination])
                break
        }
        this.updateLocation()
        this.update()
    },
    //@+node:gcross.20110627234551.1182: *3* play
    play: function(callback) {
        this.prepare()
        if(typeof this.script[this.marker] == "string" || !this.script[this.marker].duration) {
            this.advance()
            this.update()
            callback()
        } else {
            var animation = this.script[this.marker]
            animation.stepTo(this.stage,0)
            this.stage.update()
            this.animator =
                new Animator(
                    this,
                    animation,
                    function() {
                        ++self.marker
                        callback()
                    }
                )
            this.animator.start()
        }
    },
    //@+node:gcross.20110627234551.1185: *3* playSlide
    playSlide: function() {
        this.skipTagChunk()
        this.playUntilTagReached()
    },
    //@+node:gcross.20110627234551.1187: *3* playUntilTagReached
    playUntilTagReached: function() {
        if(this.marker >= this.script.length || this.atTag()) {
            this.updateLocation()
        } else {
            var self = this
            this.play(function() { self.playUntilTagReached(); })
        }
    },
    //@+node:gcross.20110627234551.1170: *3* prepare
    prepare: function() {
        if(this.marker == this.prepared) {
            if(typeof this.script[this.marker] != "string") {
                this.script[this.marker] = this.script[this.marker](this.stage)
            }
            ++this.prepared
        }
    },
    //@+node:gcross.20110627234551.1174: *3* retract
    retract: function() {
        --this.marker
        if(!this.atTag()) this.script[this.marker].retract(this.stage)
    },
    //@+node:gcross.20110627234551.1177: *3* rewind
    rewind: function(n) {
        if(n == undefined) n = 1
        if(this.animator) {
            this.animator.stop()
            delete this.animator
            this.script[this.marker].retract(this.stage)
            n -= 1
        }
        this.gotoSlide(this.getCurrentSlide()-n)
    },
    //@+node:gcross.20110627234551.1186: *3* skipTagChunk
    skipTagChunk: function() {
        while(this.atTag()) this.advance()
    },
    //@+node:gcross.20110627234551.1183: *3* update
    update: function() { this.stage.update(); },
    //@+node:gcross.20110629133112.1178: *3* updateLocation
    updateLocation: function() { window.location.hash = this.getCurrentSlide(); }
    //@-others
}
//@+node:gcross.20110627234551.1156: ** Augmentations
//@+node:gcross.20110629121436.1178: *3* appendToMethod
function appendToMethod(prototype,name,new_method) {
    var old_method = prototype[name]
    if(old_method == undefined) {
        prototype[name] = new_method
    } else {
        prototype[name] = function() {
            old_method.apply(this,arguments)
            new_method.apply(this,arguments)
        }
    }
}
//@+node:gcross.20110629121436.1179: *3* augment
function augment(cls,methods) {
    for(var name in methods)
        cls[name] = methods[name]
}
//@+node:gcross.20110629231851.1184: *3* augmentWithStyleBehavior
function augmentWithStyleBehavior(actor_class) {
    appendToMethod(actor_class,"update",function () {
        var style = ""
        for(var key in this.style) style += (key + ":" + this.style[key] + ";")
        if(this.style) {
            this.node.setAttribute("style",style)
        } else {
            this.node.removeAttribute("style")
        }
    })
}
//@+node:gcross.20110627234551.1155: *3* augmentWithTransformBehavior
function augmentWithTransformBehavior(actor_class) {
    appendToMethod(actor_class,"update",function () {
        this.node.setAttribute("transform","translate(" + this.x + "," + this.y + ")scale(" + this.scale + ")")
    })
    augment(actor_class,{x: 0, y: 0, scale: 1})
}
//@+node:gcross.20110629133112.1185: *3* chainAfterMethod
function chainAfterMethod(prototype,name,new_method) {
    var old_method = prototype[name]
    if(old_method == undefined) {
        prototype[name] = new_method
    } else {
        prototype[name] = function() {
            var old_value = old_method.apply(this,arguments)
            var new_arguments = [old_value]
            for(var i = 0; i < arguments.length; ++i) new_arguments.push(arguments[i])
            return new_method.apply(this,new_arguments)
        }
    }
}
//@+node:gcross.20110627234551.1146: ** Actors
//@+node:gcross.20110626200911.1139: *3* [ Actor prototype ]
var ActorPrototype = {
    clearNode: function() {
        delete this.node
    }

,   getNode: function() {
        if(this.node == undefined) {
            this.node = this.createNode(true)
        }
        return this.node
    }
}
//@+node:gcross.20110626200911.1135: *3* UseActor
function UseActor(id) {
    this.id = id
    this.style = {}
}
UseActor.prototype = Object.create(ActorPrototype)
augment(UseActor.prototype,{
    createNode: function() {
        var node = document.createElementNS(svg_namespace,"use")
        node.setAttributeNS(xlink_namespace,"href","#"+this.id)
        return node
    }
})
augmentWithStyleBehavior(UseActor.prototype)
augmentWithTransformBehavior(UseActor.prototype)

function hireUseActor(id,actor_name_after) { return hire(id,new UseActor(id),actor_name_after); }

function hireUseActors() {
    var hires = []
    for(var i = 0; i < arguments.length; ++i) {
        var id = arguments[i]
        hires.push(hire(id,new UseActor(id)))
    }
    return sequence.apply(null,hires)
}

function hireAndFadeInUseActor(duration,id,actor_name_after) { return hireAndFadeIn(duration,id,new UseActor(id),actor_name_after); }

function hireAndFadeInUseActors(duration) {
    var hires = []
    for(var i = 1; i < arguments.length; ++i) {
        var id = arguments[i]
        hires.push(hireAndFadeIn(duration,id,new UseActor(id)))
    }
    return parallel.apply(null,hires)
}
//@+node:gcross.20110627234551.1147: ** Animations
//@+node:gcross.20110712230720.1196: *3* PropertyAnimationPrototype
var PropertyAnimationPrototype = {
    set: function(stage,new_value) {
        this.getObjectFromStage(stage)[this.property_name] = new_value
    }
,   get: function(stage) {
        return this.getObjectFromStage(stage)[this.property_name]
    }
}
//@+node:gcross.20110702143209.1189: *3* Null
function NullAnimation(duration) {
    this.duration = duration
}
NullAnimation.prototype = {
    advance: function(stage) {}
,   retract: function(stage) {}
,   stepTo: function(stage,time) {}
}

function wait(duration) {
    return function(stage) { return new NullAnimation(duration) }
}
//@+node:gcross.20110626200911.1140: *3* Parallel
function ParallelAnimation(animations) {
    this.animations = animations
    animations.forEach(function(animation) {
        animation.duration = animation.duration || 0
    })
    animations.sort(function(a,b) { return a.duration - b.duration; })
    this.duration = animations[animations.length-1].duration
}
ParallelAnimation.prototype = {
    finished: 0
,   advance: function(stage) {
        for(var i = this.finished; i < this.animations.length; ++i)
            this.animations[i].advance(stage)
        delete this.finished
    }
,   retract: function(stage) {
        for(var i = this.animations.length-1; i >= 0; --i)
            this.animations[i].retract(stage)
        delete this.finished
    }
,   stepTo: function(stage,time) {
        for(var i = this.finished; i < this.animations.length; ++i) {
            var animation = this.animations[i]
            if(time >= animation.duration) {
                animation.advance(stage)
                ++this.finished
            } else {
                animation.stepTo(stage,time)
            }
        }
    }
}

function parallel() {
    var animation_arguments = arguments
    return function(stage) {
        var animations = []
        for(var i = 0; i < animation_arguments.length; ++i)
            animations.push(animation_arguments[i](stage))
        return new ParallelAnimation(animations)
    }
}
//@+node:gcross.20110626200911.1146: *3* Sequence
function SequenceAnimation(animations) {
    this.animations = animations
    this.duration = 0
    animations.forEach(function(animation) {
        animation.duration = animation.duration || 0
        this.duration += animation.duration
    },this)
}
SequenceAnimation.prototype = {
    finished: 0
,   advance: function(stage) {
        for(var i = this.finished; i < this.animations.length; ++i)
            this.animations[i].advance(stage)
        this.finished = this.animations.length
    }
,   retract: function(stage) {
        for(var i = Math.min(this.finished,this.animations.length-1); i >= 0; --i)
            this.animations[i].retract(stage)
        this.finished = 0
    }
,   stepTo: function(stage,time) {
        for(var i = 0;
            i < this.finished;
            time -= this.animations[i].duration, ++i
        ) ;
        for(;
            i < this.animations.length && time >= this.animations[i].duration;
            time -= this.animations[i].duration, ++i
        ) {
            this.animations[i].advance(stage)
            ++this.finished
        }
        if(i < this.animations.length) this.animations[i].stepTo(stage,time)
    }
}

function sequence() {
    var animation_arguments = arguments
    return function(stage) {
        var animations = []
        for(var i = 0; i < animation_arguments.length; ++i) {
            var animation = animation_arguments[i](stage)
            animation.advance(stage)
            animations.push(animation)
        }
        for(var i = animation_arguments.length-1; i >= 0; --i) {
            animations[i].retract(stage)
        }
        return new SequenceAnimation(animations)
    }
}
//@+node:gcross.20110629133112.1182: *3* Set
function Set(getObjectFromStage,property_name,new_value,old_value) {
    this.getObjectFromStage = getObjectFromStage
    this.property_name = property_name
    this.new_value = new_value
    this.old_value = old_value
}
Set.prototype = Object.create(PropertyAnimationPrototype)
augment(Set.prototype,{
    advance: function(stage) { this.set(stage,this.new_value) }
,   retract: function(stage) { this.set(stage,this.old_value) }
})

function set(getObjectFromStage,property_name,new_value) {
    return function(stage) {
        getObjectFromStage = convertStringToGetter(getObjectFromStage)
        return new Set(getObjectFromStage,property_name,new_value,getObjectFromStage(stage)[property_name])
    }
}
//@+node:gcross.20110711171503.1272: *3* Remove
function Remove(getObjectFromStage,property_name,old_value) {
    this.getObjectFromStage = getObjectFromStage
    this.property_name = property_name
    this.old_value = old_value
}
Remove.prototype = Object.create(PropertyAnimationPrototype)
augment(Remove.prototype,{
    advance: function(stage) {
        var object = this.getObjectFromStage(stage)
        if(CSSStyleDeclaration.prototype.isPrototypeOf(object))
            object[this.property_name] = ""
        else
            delete object[this.property_name]
    }
,   retract: function(stage) { this.set(stage,this.old_value) }
})

function remove(getObjectFromStage,property_name) {
    return function(stage) {
        getObjectFromStage = convertStringToGetter(getObjectFromStage)
        return new Remove(getObjectFromStage,property_name,getObjectFromStage(stage)[property_name])
    }
}
//@+node:gcross.20110629133112.1188: *3* Interpolating
function InterpolatingAnimation(easing,duration,getObjectFromStage,property_name,old_value,starting_value,ending_value) {
    starting_value = Number(starting_value)
    ending_value = Number(ending_value)
    this.ease = easing
    this.duration = duration
    this.getObjectFromStage = getObjectFromStage
    this.property_name = property_name
    this.old_value = old_value
    this.new_value = ending_value
    this.base = starting_value
    this.delta = ending_value - starting_value
}
InterpolatingAnimation.prototype = Object.create(PropertyAnimationPrototype)
augment(InterpolatingAnimation.prototype,{
    advance: function(stage) { this.set(stage,this.new_value) }
,   retract: function(stage) { this.set(stage,this.old_value) }
,   stepTo: function(stage,time) { this.set(stage,this.base + this.ease(time/this.duration) * this.delta) }
})

function interpolate(easing,duration,getObjectFromStage,property_name,v1,v2) {
    return function(stage) {
        getObjectFromStage = convertStringToGetter(getObjectFromStage)
        var old_value = getObjectFromStage(stage)[property_name]
        var starting_value, ending_value
        if(v2 == undefined) {
            starting_value = old_value
            ending_value = v1
        } else {
            starting_value = v1
            ending_value = v2
        }
        return new
            InterpolatingAnimation(
                easing,
                duration,
                getObjectFromStage,
                property_name,
                old_value,
                starting_value,
                ending_value
            )
    }
}

function makeInterpolater(easing) {
    return function(duration,getObjectFromStage,property_name,v1,v2) {
        return interpolate(easing,duration,getObjectFromStage,property_name,v1,v2)
    }
}
//@+node:gcross.20110629233843.1185: *3* Fading
function fadeOut(duration,getObjectFromStage,starting_opacity) {
    getObjectFromStage = convertStringToGetter(getObjectFromStage)
    return function(stage) {
        var old_opacity = getObjectFromStage(stage).style.opacity
        if(starting_opacity == undefined)
            starting_opacity = old_opacity
        if(starting_opacity == undefined || starting_opacity == "")
            starting_opacity = 1
        return new InterpolatingAnimation(
                function(t) { return t; },
                duration,
                function(stage) { return getObjectFromStage(stage).style; },
                "opacity",
                old_opacity,
                starting_opacity,
                0
            )
    }
}

function fadeIn(duration,getObjectFromStage,current_opacity) {
    getObjectFromStage = convertStringToGetter(getObjectFromStage)
    return linear(
        duration,
        function(stage) { return getObjectFromStage(stage).style; },
        "opacity",
        0,
        1
    )
}
//@+node:gcross.20110627234551.1162: ** Cast changes
//@+node:gcross.20110627234551.1163: *3* Hire
function Hire(name,actor,actor_name_after) {
    this.name = name
    this.actor = actor
    this.actor_name_after = actor_name_after
}
Hire.prototype = {
    advance: function(stage) {
        stage.insertActorBefore(this.name,this.actor,this.actor_name_after)
    }

,   retract: function(stage) {
        stage.removeActor(this.name)
        this.actor.clearNode()
    }
}

function hire(name,actor,actor_name_after) {
    if(!actor) actor = UseActor
    if(typeof actor == "function") actor = new actor(name)
    return function(stage) {
        return new Hire(name,actor,actor_name_after)
    }
}

function hireAndFadeIn(duration,name,actor,actor_name_after) {
    if(!actor) actor = UseActor
    if(typeof actor == "function") actor = new actor(name)
    return sequence(
        hire(name,actor,actor_name_after),
        fadeIn(duration,name)
    )
}
//@+node:gcross.20110627234551.1167: *3* Fire
function Fire(name,actor,actor_name_after) {
    this.name = name
    this.actor = actor
    this.actor_name_after = actor_name_after
}
Fire.prototype = {
    advance: function(stage) {
        stage.removeActor(this.name)
        this.actor.clearNode()
    }

,   retract: function(stage) {
        stage.insertActorBefore(this.name,this.actor,this.actor_name_after)
    }
}

function fire() {
    var names = arguments
    return function(stage) {
        var excluding = {}
        var animations = []
        for(var i = 0; i < names.length; ++i) {
            var name = names[i]
            animations.push(new Fire(name,stage.getActor(name),stage.getActorNameAfter(name,excluding)))
            excluding[name] = 1
        }
        return new SequenceAnimation(animations)
    }
}

function fadeOutAndFire(duration) {
    var names = []
    for(var i = 1; i < arguments.length; ++i) names.push(arguments[i])
    return sequence(
        parallel.apply(null,names.map(function(name) { return fadeOut(duration,name); })),
        fire.apply(null,names)
    )
}
//@+node:gcross.20110702143209.1200: *3* MoveInStack
function MoveInStack(name,old_name_after,new_name_after) {
    this.name = name
    this.old_name_after = old_name_after
    this.new_name_after = new_name_after
}
MoveInStack.prototype = {
    advance: function(stage) {
        stage.moveActorBefore(this.name,this.new_name_after)
    }

,   retract: function(stage) {
        stage.moveActorBefore(this.name,this.old_name_after)
    }
}

function moveBefore(name,name_after) {
    return function(stage) {
        return new MoveInStack(name,stage.getActorNameAfter(name),name_after)
    }
}

function moveToEnd(name) {
    return function(stage) {
        return new MoveInStack(name,stage.getActorNameAfter(name))
    }
}
//@+node:gcross.20110629221709.1183: ** Interpolations
var linear = makeInterpolater(function(t) { return t; })
var smooth = makeInterpolater(function(t) { var x = Math.sin(Math.PI*t/2); return x*x; })
var decelerate = makeInterpolater(function(t) { return Math.sin(Math.PI*t/2); })
var accelerate = makeInterpolater(function(t) { return 1-Math.cos(Math.PI*t/2); })
//@-others
//@-leo
