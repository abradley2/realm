@module("morphdom")
external _morphdom: (Dom.window, Dom.htmlElement, Dom.htmlElement) => unit = "call"
let update = _morphdom(HTMLElement.window)
