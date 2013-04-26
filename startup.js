// Startup {{{
window.addEventListener("load",function() {
    (function() {
        var resources = document.getElementById("resources")
        var heading_template = document.getElementById("heading_template")
        for(var i = 0; i < headings.length; ++i) {
            var heading = headings[i]
            var node = heading_template.cloneNode(false)
            node.setAttribute("id",heading)
            node.appendChild(document.createTextNode(heading))
            resources.appendChild(node)
        }
    })()

    initializeSlick([
        hire("title_slide"),
        "",
        fadeOutAndFire(1,"title_slide"),
        hireAndFadeInUseActors(0.5,
            "standard_backdrop_bottom",
            "standard_backdrop_top",
            headings[nextHeadingIndex()]
        ),
    ].concat(script()))
},false)
// }}}
