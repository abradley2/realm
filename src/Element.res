open Belt
open Property
open Event
open Attribute

type rec node<'msg> = {
  tag: string,
  content: option<string>,
  properties: list<property<'msg>>,
  children: list<node<'msg>>,
}

let createNode = (tag: string, properties: list<property<'msg>>, children: list<node<'msg>>): node<
  'msg,
> => {
  tag: tag,
  content: None,
  properties: properties,
  children: children,
}

let text = content => {
  tag: "__text__",
  content: Some(content),
  properties: list{},
  children: list{},
}

type element =
  | Element(Dom.htmlElement)
  | Text(Dom.text)

type rec streamElement<'msg> = {
  el: element,
  children: list<streamElement<'msg>>,
  stream: Most.stream<'msg>,
}

let rec combineStreams = all => {
  switch all {
  | list{} => Most.empty()
  | list{x, ...xs} => Most.merge(x, combineStreams(xs))
  }
}

let rec addChildren = (el, children) => {
  switch children {
  | list{} => ()
  | list{Element(child), ...next} =>
    HTMLElement.appendChild(el, child)->(() => addChildren(el, next))
  | list{Text(child), ...next} => HTMLElement.appendText(el, child)->(() => addChildren(el, next))
  }
}

let rec addAttributes = (target, props) => {
  switch props {
  | list{Attribute(attr), ...next} =>
    attr->applyAttribute(target)->(() => addAttributes(target, next))
  | list{_props, ...next} => addAttributes(target, next)
  | list{} => ()
  }
}

let rec addEvents = (target, props) => {
  switch props {
  | list{Event(ev), ...next} => ev->applyEvent(target)->Most.merge(addEvents(target, next))
  | list{_prop, ...next} => addEvents(target, next)
  | list{} => Most.empty()
  }
}

let rec createElement = (n: node<'msg>): streamElement<'msg> => {
  switch n.tag {
  | "__text__" => {
      let el = n.content->Option.getWithDefault("")->HTMLElement.createTextNode

      let stream = Most.empty()

      {
        el: Text(el),
        children: list{},
        stream: stream,
      }
    }
  | tag => {
      let el = HTMLElement.createElement(tag)

      let children = n.children->List.map(createElement)
      let stream = addEvents(el, n.properties)

      addChildren(el, children->List.map(child => child.el))
      addAttributes(el, n.properties)

      {
        el: Element(el),
        children: children,
        stream: children->List.map(child => child.stream)->combineStreams->Most.merge(stream),
      }
    }
  }
}

let render = (n: node<'ms>, el: Dom.htmlElement): Most.stream<'msg> => {
  let withStream = createElement(n)

  switch withStream.el {
  | Element(e) => HTMLElement.appendChild(el, e)
  | Text(t) => HTMLElement.appendText(el, t)
  }->(() => withStream.stream)
}


let div = createNode("div", _)
let div' = createNode("div", _, list{})
let div_ = createNode("div", list{}, _)

let span = createNode("span", _)
let span' = createNode("span", _, list{})
let span_ = createNode("span", list{}, _)

let button = createNode("button", _)
let button' = createNode("button", _, list{})
let button_ = createNode("button", list{}, _)

let p = createNode("p", _)
let p' = createNode("p", _, list{})
let p_ = createNode("p", list{}, _)

let label = createNode("label", _)
let label' = createNode("label", _, list{})
let label_ = createNode("label", list{}, _)

let h1 = createNode("h1", _)
let h1' = createNode("h1", _, list{})
let h1_ = createNode("h1", list{}, _)

let h2 = createNode("h2", _)
let h2' = createNode("h2", _, list{})
let h2_ = createNode("h2", list{}, _)

let h3 = createNode("h3", _)
let h3' = createNode("h3", _, list{})
let h3_ = createNode("h3", list{}, _)

let h4 = createNode("h4", _)
let h4' = createNode("h4", _, list{})
let h4_ = createNode("h4", list{}, _)

let h5 = createNode("h5", _)
let h5' = createNode("h5", _, list{})
let h5_ = createNode("h5", list{}, _)

let h6 = createNode("h6", _)
let h6' = createNode("h6", _, list{})
let h6_ = createNode("h6", list{}, _)

let b = createNode("b", _)
let b' = createNode("b", _, list{})
let b_ = createNode("b", list{}, _)

let i = createNode("i", _)
let i' = createNode("i", _, list{})
let i_ = createNode("i", list{}, _)

let small = createNode("small", _)
let small' = createNode("small", _, list{})
let small_ = createNode("small", list{}, _)

let hr = createNode("hr", _)
let hr' = createNode("hr", _, list{})
let hr_ = createNode("hr", list{}, _)

let pre = createNode("pre", _)
let pre' = createNode("pre", _, list{})
let pre_ = createNode("pre", list{}, _)

let br = createNode("br", list{}, list{})

let ul = createNode("ul", _)
let ul' = createNode("ul", _, list{})
let ul_ = createNode("ul", list{}, _)

let ol = createNode("ol", _)
let ol' = createNode("ol", _, list{})
let ol_ = createNode("ol", list{}, _)

let li = createNode("li", _)
let li' = createNode("li", _, list{})
let li_ = createNode("li", list{}, _)

let img = createNode("img", _)
let img' = createNode("img", _, list{})
let img_ = createNode("img", list{}, _)

let form = createNode("form", _)
let form' = createNode("form", _, list{})
let form_ = createNode("form", list{}, _)

let textarea = createNode("textarea", _)
let textarea' = createNode("textarea", _, list{})
let textarea_ = createNode("textarea", list{}, _)

let select = createNode("select", _)
let select' = createNode("select", _, list{})
let select_ = createNode("select", list{}, _)

let option = createNode("option", _)
let option' = createNode("option", _, list{})
let option_ = createNode("option", list{}, _)

let table = createNode("table", _)
let table' = createNode("table", _, list{})
let table_ = createNode("table", list{}, _)

let thead = createNode("thead", _)
let thead' = createNode("thead", _, list{})
let thead_ = createNode("thead", list{}, _)

let tbody = createNode("tbody", _)
let tbody' = createNode("tbody", _, list{})
let tbody_ = createNode("tbody", list{}, _)

let tr = createNode("tr", _)
let tr' = createNode("tr", _, list{})
let tr_ = createNode("tr", list{}, _)

let td = createNode("td", _)
let td' = createNode("td", _, list{})
let td_ = createNode("td", list{}, _)

let th = createNode("th", _)
let th' = createNode("th", _, list{})
let th_ = createNode("th", list{}, _)

let fieldset = createNode("fieldset", _)
let fieldset' = createNode("fieldset", _, list{})
let fieldset_ = createNode("fieldset", list{}, _)

let audio = createNode("audio", _)
let audio' = createNode("audio", _, list{})
let audio_ = createNode("audio", list{}, _)

let video = createNode("video", _)
let video' = createNode("video", _, list{})
let video_ = createNode("video", list{}, _)

let source = createNode("source", _)
let source' = createNode("source", _, list{})
let source_ = createNode("source", list{}, _)

let track = createNode("track", _)
let track' = createNode("track", _, list{})
let track_ = createNode("track", list{}, _)
