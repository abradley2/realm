open Property

let applyAttribute = (attr: attribute, target: Dom.htmlElement): unit => {
  switch attr {
  | Id(val) => HTMLElement.setId(target, val)
  | ClassName(val) => HTMLElement.setClassName(target, val)
  }
}
