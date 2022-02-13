open Belt
open Property
open Event
open Attribute
open HTMLElement

type rec vnode<'msg> = {
  tag: string,
  content: option<string>,
  properties: list<property<'msg>>,
  children: list<vnode<'msg>>,
}

type rec virtualElement<'msg> = {
  el: HTMLElement.node,
  vnode: vnode<'msg>,
  children: list<virtualElement<'msg>>,
}

let createVnode = (
  tag: string,
  properties: list<property<'msg>>,
  children: list<vnode<'msg>>,
): vnode<'msg> => {
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

let rec addChildren = (el, children) => {
  switch children {
  | list{} => ()
  | list{Element(child), ...next} =>
    HTMLElement.appendChild(el, child->liftElement)->(() => addChildren(el, next))
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

let rec createElement = (n: vnode<'msg>): virtualElement<'msg> => {
  switch n.tag {
  | "__text__" => {
      let el = n.content->Option.getWithDefault("")->HTMLElement.createTextNode

      {
        el: Text(el),
        children: list{},
        vnode: n,
      }
    }
  | tag => {
      let el = HTMLElement.createElement(tag)

      let children = n.children->List.map(createElement)

      addChildren(el, children->List.map(child => child.el))
      addAttributes(el, n.properties)

      {
        el: Element(el),
        children: children,
        vnode: n,
      }
    }
  }
}

let div = createVnode("div", _)
let div' = createVnode("div", _, list{})
let div_ = createVnode("div", list{}, _)

let span = createVnode("span", _)
let span' = createVnode("span", _, list{})
let span_ = createVnode("span", list{}, _)

let button = createVnode("button", _)
let button' = createVnode("button", _, list{})
let button_ = createVnode("button", list{}, _)

let p = createVnode("p", _)
let p' = createVnode("p", _, list{})
let p_ = createVnode("p", list{}, _)

let label = createVnode("label", _)
let label' = createVnode("label", _, list{})
let label_ = createVnode("label", list{}, _)

let h1 = createVnode("h1", _)
let h1' = createVnode("h1", _, list{})
let h1_ = createVnode("h1", list{}, _)

let h2 = createVnode("h2", _)
let h2' = createVnode("h2", _, list{})
let h2_ = createVnode("h2", list{}, _)

let h3 = createVnode("h3", _)
let h3' = createVnode("h3", _, list{})
let h3_ = createVnode("h3", list{}, _)

let h4 = createVnode("h4", _)
let h4' = createVnode("h4", _, list{})
let h4_ = createVnode("h4", list{}, _)

let h5 = createVnode("h5", _)
let h5' = createVnode("h5", _, list{})
let h5_ = createVnode("h5", list{}, _)

let h6 = createVnode("h6", _)
let h6' = createVnode("h6", _, list{})
let h6_ = createVnode("h6", list{}, _)

let b = createVnode("b", _)
let b' = createVnode("b", _, list{})
let b_ = createVnode("b", list{}, _)

let i = createVnode("i", _)
let i' = createVnode("i", _, list{})
let i_ = createVnode("i", list{}, _)

let small = createVnode("small", _)
let small' = createVnode("small", _, list{})
let small_ = createVnode("small", list{}, _)

let hr = createVnode("hr", _)
let hr' = createVnode("hr", _, list{})
let hr_ = createVnode("hr", list{}, _)

let pre = createVnode("pre", _)
let pre' = createVnode("pre", _, list{})
let pre_ = createVnode("pre", list{}, _)

let br = createVnode("br", list{}, list{})

let ul = createVnode("ul", _)
let ul' = createVnode("ul", _, list{})
let ul_ = createVnode("ul", list{}, _)

let ol = createVnode("ol", _)
let ol' = createVnode("ol", _, list{})
let ol_ = createVnode("ol", list{}, _)

let li = createVnode("li", _)
let li' = createVnode("li", _, list{})
let li_ = createVnode("li", list{}, _)

let img = createVnode("img", _)
let img' = createVnode("img", _, list{})
let img_ = createVnode("img", list{}, _)

let form = createVnode("form", _)
let form' = createVnode("form", _, list{})
let form_ = createVnode("form", list{}, _)

let textarea = createVnode("textarea", _)
let textarea' = createVnode("textarea", _, list{})
let textarea_ = createVnode("textarea", list{}, _)

let select = createVnode("select", _)
let select' = createVnode("select", _, list{})
let select_ = createVnode("select", list{}, _)

let option = createVnode("option", _)
let option' = createVnode("option", _, list{})
let option_ = createVnode("option", list{}, _)

let table = createVnode("table", _)
let table' = createVnode("table", _, list{})
let table_ = createVnode("table", list{}, _)

let thead = createVnode("thead", _)
let thead' = createVnode("thead", _, list{})
let thead_ = createVnode("thead", list{}, _)

let tbody = createVnode("tbody", _)
let tbody' = createVnode("tbody", _, list{})
let tbody_ = createVnode("tbody", list{}, _)

let tr = createVnode("tr", _)
let tr' = createVnode("tr", _, list{})
let tr_ = createVnode("tr", list{}, _)

let td = createVnode("td", _)
let td' = createVnode("td", _, list{})
let td_ = createVnode("td", list{}, _)

let th = createVnode("th", _)
let th' = createVnode("th", _, list{})
let th_ = createVnode("th", list{}, _)

let fieldset = createVnode("fieldset", _)
let fieldset' = createVnode("fieldset", _, list{})
let fieldset_ = createVnode("fieldset", list{}, _)

let audio = createVnode("audio", _)
let audio' = createVnode("audio", _, list{})
let audio_ = createVnode("audio", list{}, _)

let video = createVnode("video", _)
let video' = createVnode("video", _, list{})
let video_ = createVnode("video", list{}, _)

let source = createVnode("source", _)
let source' = createVnode("source", _, list{})
let source_ = createVnode("source", list{}, _)

let track = createVnode("track", _)
let track' = createVnode("track", _, list{})
let track_ = createVnode("track", list{}, _)
