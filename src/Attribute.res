open Property

let applyAttribute = (attr: attribute, target: Dom.htmlElement): unit => {
  switch attr {
  | Id(val) => HTMLElement.setId(target, val)
  | ClassName(val) => HTMLElement.setClassName(target, val)
  | Value(val) =>
    switch HTMLElement.isInput(target) {
    | Some(input) => {
      HTMLElement.setAttribute(target, "value", val)
      HTMLElement.setValue(input, val)
    }
    | None => HTMLElement.setAttribute(target, "value", val)
    }
  | Data(key, val) => HTMLElement.setAttribute(target, key, val)
  }
}

let readAttribute = (key: string, val: string): attribute => {
  switch key {
  | "id" => Id(val)
  | "class" => ClassName(val)
  | "value" => Value(val)
  | _ => Data(key, val)
  }
}
