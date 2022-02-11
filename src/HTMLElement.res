open Belt

// Document Functionality
@val
external window: Dom.window = "window"

@val @scope("window")
external document: Dom.htmlElement = "document"

@val @scope(("window", "document"))
external body: Dom.htmlElement = "body"

@val @scope(("window", "document"))
external createElement: string => Dom.htmlElement = "createElement"

@val @scope(("window", "document"))
external createTextNode: string => Dom.text = "createTextNode"

@send
external _querySelector: (Dom.htmlElement, string) => Js.null_undefined<Dom.htmlElement> =
  "querySelector"
let querySelector = (el, q) => _querySelector(el, q)->Js.toOption

// Transform Elements
@send
external insertBefore: (Dom.htmlElement, Dom.htmlElement) => unit = "insertBefore"

@send
external appendChild: (Dom.htmlElement, Dom.htmlElement) => unit = "appendChild"

@send
external appendText: (Dom.htmlElement, Dom.text) => unit = "appendChild"

@send
external _getAttribute: (Dom.htmlElement, string) => Js.null_undefined<string> = "getAttribute"
let getAttribute = (el, attr) => _getAttribute(el, attr)->Js.toOption

@send
external setAttribute: (Dom.htmlElement, string, string) => unit = "setAttribute"

@send
external removeAttribute: (Dom.htmlElement, string) => unit = "removeAttribute"

// Add/Remove Event Listeners

@send
external _addEventListener: (Dom.htmlElement, string, Dom.event => unit) => unit =
  "addEventListener"

@send
external removeEventListener: (Dom.htmlElement, string, Dom.event => unit) => unit =
  "removeEventListener"

let addEventListener = (el, ev, handler) => {
  let _bindEvent = _addEventListener(el, ev, handler)
  () => removeEventListener(el, ev, handler)
}

// Check Class
type node =
  | Element(Dom.htmlElement)
  | Text(Dom.text)

let mapElement = (n, fn) =>
  switch n {
  | Element(el) => fn(el)->Some
  | Text(_) => None
  }

let mapText = (n, fn) =>
  switch n {
  | Element(_) => None
  | Text(el) => fn(el)->Some
  }

let _isElement: Dom.node => Js.nullable<
  Dom.htmlElement,
> = %raw(`function (el) { if (el instanceof HTMLElement) { return el }; return null }`)
let isElement = el => _isElement(el)->Js.toOption

let _isText: Dom.node => Js.nullable<
  Dom.text,
> = %raw(`function (el) { if (el instanceof Text) { return el }; return null }`)
let isText = el => _isText(el)->Js.toOption

let node = (el: Dom.node) =>
  switch (isElement(el), isText(el)) {
  | (Some(el), _) => Element(el)
  | (_, Some(el)) => Text(el)
  | _ => Js.Exn.raiseError("Invalid node")
  }

let _isInput: Dom.htmlElement => Js.nullable<
  Dom.htmlInputElement,
> = %raw(`function (el) { if (el instanceof HTMLInputElement) { return el }; return null }`)
let isInput = el => _isInput(el)->Js.toOption

// Event Getters

@get external getTarget: Dom.event => Dom.htmlElement = "target"

// Attribute Getters and Setters

@get external _getChildNodes: Dom.htmlElement => Js.Array2.array_like<Dom.node> = "childNodes"
let getChildNodes = el => _getChildNodes(el)->Js.Array2.from->Js.Array2.map(node)

@get external getValue: Dom.htmlInputElement => string = "value"

@set external setValue: (Dom.htmlInputElement, string) => unit = "value"

@set external setClassName: (Dom.htmlElement, string) => unit = "className"

@get external getId: Dom.htmlElement => string = "id"

@set external setId: (Dom.htmlElement, string) => unit = "id"

@get external getTextContent: Dom.text => string = "textContent"

@set external setTextContent: (Dom.text, string) => unit = "textContent"

@get external _getNextSibling: Dom.htmlElement => Js.nullable<Dom.node> = "nextSibling"
let getNextSibling = el => _getNextSibling(el)->Js.toOption->Option.map(node)

type attr = {
  name: string,
  value: string,
}

@get external _getAttributes: Dom.htmlElement => Js.Array2.array_like<attr> = "attributes"
let getAttributes = el => _getAttributes(el)->Js.Array2.from

let getAttributesMap = el =>
  _getAttributes(el)->Js.Array2.from->Js.Array2.reduce((attrMap, attr) => {
    attrMap->HashMap.String.set(attr.name, attr.value)

    attrMap
  }, HashMap.String.fromArray([]))
