import os
import os.path
from xml.etree.ElementTree import Element, ElementTree, parse

svgns = "{http://www.w3.org/2000/svg}"

output = Element(
    svgns + "svg",
    {},
    height="768",
    width="1024",
    version="1.1",
    style="background-color:black",
)

for script_name in ["slick.js","startup.js","script.js"]:
    output.append(Element(svgns + "script",{"{http://www.w3.org/1999/xlink}href":script_name}))

defs = Element(svgns + "defs",{"id":"resources"})

for filename in os.listdir("resources"):
    if not filename.endswith(".svg"): continue
    print("Parsing " + filename + "...")
    tree = parse(os.path.join("resources",filename))
    root = tree.getroot()
    for child in root:
        if child.tag.startswith("{http://www.w3.org/2000/svg}"):
            defs.append(child)

output.append(defs)

ElementTree(output).write("talk.svg")
