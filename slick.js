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

var director
//@-<< Global variables >>

//@+others
//@+node:gcross.20110627234551.1201: ** Initialization
//@+node:gcross.20110627234551.1180: *3* function initializeSlick
function initialize() {
    director = new Director(script)
    
    window.addEventHandler("keydown",function(event) {
        switch(event.keyCode || event.charCode) {
            case SPACE_KEY:
                if(director.animator) {
                    var animator = director.animator
                    if(animator.active())
                        animator.pause()
                    else
                        animator.start()
                } else {
                    director.play()
                }
                break
            case LEFT_KEY:
                director.rewind()
            case RIGHT_KEY:
                director.fastforward()
            case UP_KEY:
                director.rewind(10)
            case DOWN_KEY:
                director.fastforward(10)
        }
    },false)
    
    if(window.location.hash) {
        director.gotoSlide(window.location.hash.substring(1))
    } else {
        director.play()
    }
}
//@+node:gcross.20110627234551.1203: *3* function installSlick
function installSlick() { window.addEventListener("load",initializeSlick,false); }
//@+node:gcross.20110626200911.1122: ** class Stage
function Stage() {
    this.nodes = {}
    this.ordering = []
}
Stage.prototype = {
    //@+others
    //@+node:gcross.20110626200911.1134: *3* addActor
    addActor: function(name,actor) {
        this[name] = actor
        var node = actor.getNode()
        nodes[name] = node
        return node
    },
    //@+node:gcross.20110626200911.1124: *3* appendActor
    appendActor: function(name,actor) {
        this.ordering.push(name)
        this.node.appendChild(addActor(name,actor))
    },
    //@+node:gcross.20110627234551.1169: *3* getActor
    getActor: function(name) { return this[name]; },
    //@+node:gcross.20110627234551.1165: *3* getActorNameAfter
    getActorNameAfter: function(name) {
        var index = this.ordering.indexOf(name)
        if(index < this.ordering.length-1)
            return this.ordering[index+1]
        else
            return null
    },
    //@+node:gcross.20110627234551.1159: *3* getNode
    getNode: function() {
        if(this.getNode == undefined) {
            this.node = document.createElementNS(svg_namespace,"g")
            for(name in this.ordering) {
                var node = this[name].getNode()
                nodes[name] = node
                this.node.appendChild(node)
            }
        }
        return this.node
    },
    //@+node:gcross.20110626200911.1125: *3* insertActorBefore
    insertActorBefore: function(name,actor,before_name) {
        if(before_name) {
            this.node.insertBefore(addActor(name,actor), nodes[before_name])
            this.ordering.splice(this.ordering.indexOf(before_name),0,name)
        } else {
            appendActor(name,actor)
        }
    },
    //@+node:gcross.20110626200911.1129: *3* removeActor
    removeActor: function(name) {
        var actor = this[name]
        delete this[name]
        this.node.removeChild(actor.node)
        this.ordering.splice(this.ordering.indexOf(name),1)
        return actor
    },
    //@+node:gcross.20110626200911.1133: *3* update
    update: function() {
        for(name in this.ordering) this[name].update()
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
    active: function() { return (handler_id in this); },
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
        this.starting_time = new Date()
        this.handler_id = window.setInterval(function() { this.step(); },this.interval*1000)
    },
    //@+node:gcross.20110627234551.1192: *3* step
    step: function() {
        var animation = this.animation
        var current_time = (new Date() - this.starting_time + offset) / 1000
        if(current_time < animation.duration) {
            var stage = this.director.stage
            animation.stepTo(stage,current_time)
            stage.update()
        } else {
            this.director.stop()
            this.director.advance()
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
    this.tags = {}
    this.script_slides = []
    this.slides = [0]
    var slide = 0
    var tag_chunk_in_progress = false
    for(var index = 0; index < script.length; ++index) {
        if(typeof script[index] == "string") {
            if(script[index]) tags[script[index]] = index
            tag_chunk_in_progress = true
        } else {
            if(tag_chunk_in_progress) {
                ++slide
                this.slides.push(index)
                tag_chunk_in_progress = false
            }
        }
        this.script_slides[index] = slide
    }

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
        this.gotoSlide(this.script_slides[this.marker]-n)
    },
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
        this.stop()
        switch(typeof destination) {
            case 'number':
                destination = Math.max(0,Math.min(this.slides.length-1,destination))
                this.gotoIndex(this.slides[destination])
                break
            case 'string':
                this.gotoIndex(this.tags[destination])
                break
        }
        window.location.hash = destination
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
            this.animator =
                new Animator(
                    this,
                    this.script[this.marker],
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
        this.playUntilTag()
    },
    //@+node:gcross.20110627234551.1187: *3* playUntilTagReached
    playUntilTagReached: function() {
        if(!this.atTag()) {
            var self = this
            play(function() { self.playUntilTagReached(); })
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
        if(!atTag()) this.script[this.marker].retract(this.stage)
        --this.marker
    },
    //@+node:gcross.20110627234551.1177: *3* rewind
    rewind: function(n) {
        if(n == undefined) n = 1
        this.gotoSlide(this.script_slides[this.marker]-n)
    },
    //@+node:gcross.20110627234551.1186: *3* skipTagChunk
    skipTagChunk: function() {
        while(this.atTag()) advance()
    },
    //@+node:gcross.20110627234551.1184: *3* stop
    stop: function() {
        if(this.animator) {
            this.animator.stop()
            delete this.animator
        }
    },
    //@+node:gcross.20110627234551.1183: *3* update
    update: function() { this.stage.update(); }
    //@-others
}
//@+node:gcross.20110627234551.1156: ** Augmentations
//@+node:gcross.20110627234551.1155: *3* PositionBehaviour
function augmentWithPositionBehavior(actor_class) {
    var old_update = actor_class.update
    actor_class.update = function() {
        if(old_update) old_update.call(this)
        node.setAttribute(svg_namespace,"x",this.x)
        node.setAttribute(svg_namespace,"y",this.y)
    }
    //augmentWithPropertySerialization(actor_class,"x","y")
    augmentWithPrototype(actor_class,{x: 0, y: 0})
}
//@+node:gcross.20110627234551.1157: *3* PropertySerialization
//@+at
// function augmentWithPropertySerialization(actor_class) {
//     var old_save = actor_class.save
//     actor_class.save = function() {
//         var data = old_save ? old_save.call(this) : {}
//         for(int i = 1; i < arguments.length; ++i) data[arguments[i]] = this[arguments[i]]
//         return data
//     }
//     var old_restore = actor_class.restore
//     actor_class.restore = function(data) {
//         if(old_restore) old_restore.call(this,data)
//         for(int i = 1; i < arguments.length; ++i) this[arguments[i]] = data[arguments[i]]
//     }
// }
//@+node:gcross.20110627234551.1158: *3* Prototype
function augmentWithPrototype(cls,new_intermediate_prototype) {
    new_intermediate_prototype.prototype = cls.prototype
    cls.prototype = new_intermediate_prototype
}
//@+node:gcross.20110627234551.1146: ** Actors
//@+node:gcross.20110626200911.1139: *3* [ Actor prototype ]
var ActorPrototype = {
    clearNode: function() {
        delete this.node
    }

,   getNode: function() {
        if(this.node == undefined) {
            this.node = createNode(true)
        }
        return this.node
    }
}
//@+node:gcross.20110626200911.1135: *3* UseActor
function UseActor(id) { this.id = id }
UseActor.prototype = {
    prototype: ActorPrototype

,   createNode: function() {
        var node = document.createElementNS(svg_namespace,"use")
        node.setAttribute(xlink_namespace,"href","#"+this.id)
        return node
    }
}
augmentWithPositionBehavior(UseActor)
//@+node:gcross.20110627234551.1147: ** Animations
//@+node:gcross.20110627234551.1151: *3* [ Animation prototype ]
var AnimationPrototype = {
    advance: function(stage) {
        this.setTo(stage,duration)
    }

,   retract: function(stage) {
        this.setTo(stage,0)
    }
}
//@+node:gcross.20110626200911.1140: *3* AnimationsInParallel
function AnimationsInParallel(animations) {
    var duration = 0
    for(animation in animations)
        if(animation.duration > duration)
            duration = animation.duration
    this.duration = duration
}
AnimationsInParallel.prototype = {
    prototype: AnimationPrototype

,   stepTo: function(stage,time) {
        for(animation in animations) {
            animation.stepTo(stage,Math.max(time,animation.duration))
        }
    }
}
//@+node:gcross.20110626200911.1146: *3* AnimationsInSequence
function AnimationsInSequence(animations) {
    var duration = 0
    for(animation in animations)
        duration += animation.duration
    this.duration = duration
}
AnimationsInSequence.prototype = {
    prototype: AnimationPrototype

,   stepTo: function(stage,time) {
        for(animation in animations) {
            var next_time = time - animation.duration
            if(next_time < 0) {
                animation.stepTo(stage,time)
                return
            } else {
                time = next_time
            }
        }
        var animation = animations[animations-length]
        animation.stepTo(stage,animation.duration)
    }
}
//@+node:gcross.20110627234551.1162: ** Cast changes
//@+node:gcross.20110627234551.1163: *3* Hire
function Hire(name,actor,actor_name_after) {
    this.name = name
    this.actor = actor
    this.actor_name_after
}
Hire.prototype = {
    advance: function(stage) {
        stage.insertActorBefore(name,actor,actor_name_after)
    }

,   retract: function(stage) {
        stage.removeActor(name)
        this.actor.clearNode()
    }
}

function hire(name,actor,actor_name_after) {
    return function(stage) {
        return new Hire(name,actor,actor_name_after)
    }
}
//@+node:gcross.20110627234551.1167: *3* Fire
function Fire(name,actor,actor_name_after) {
    this.name = name
    this.actor = actor
    this.actor_name_after
}
Fire.prototype = {
    advance: function(stage) {
        stage.removeActor(name)
        this.actor.clearNode()
    }

,   retract: function(stage) {
        stage.insertActorBefore(name,actor,actor_name_after)
    }
}

function fire(name) {
    return function(stage) {
        return new Fire(name,stage.getActor(name),stage.getActorNameAfter(name))
    }
}
//@-others
//@-leo
