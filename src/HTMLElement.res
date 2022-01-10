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
external appendChild: (Dom.htmlElement, Dom.htmlElement) => unit = "appendChild"

@send
external appendText: (Dom.htmlElement, Dom.text) => unit = "appendChild"

@send
external _getAttribute: (Dom.htmlElement, string) => Js.null_undefined<string> = "getAttribute"
let getAttribute = (el, attr) => _getAttribute(el, attr)->Js.toOption

@send
external setAttribute: (Dom.htmlElement, string, string) => unit = "setAttribute"

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

let _isInput: Dom.htmlElement => Js.null_undefined<
  Dom.htmlInputElement,
> = %raw(`function (el) { if (el instanceof HTMLInputElement) return el }`)
let isInput = el => _isInput(el)->Js.toOption

// Event Getters

@get external getTarget: Dom.event => Dom.htmlElement = "target"

// Attribute Getters and Setters

@get external _getChildren: Dom.htmlElement => Js.Array.array_like<Dom.htmlElement> = "children"
let getChildren = el => _getChildren(el)->Js.Array.from

@get external getValue: Dom.htmlInputElement => string = "value"

@set external setValue: (Dom.htmlInputElement, string) => unit = "value"

@set external setClassName: (Dom.htmlElement, string) => unit = "className"

@get external getId: Dom.htmlElement => string = "id"

@set external setId: (Dom.htmlElement, string) => unit = "id"
