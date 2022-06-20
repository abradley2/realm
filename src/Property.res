open Belt

type attribute =
  | Id(string)
  | ClassName(string)
  | Data(string, string)

type event<'msg> =
  | OnClick('msg)
  | OnInput(string => 'msg)

let mapEvent = (e: event<'a>, fn: 'a => 'b): event<'b> => {
  switch e {
  | OnClick(msg) => fn(msg)->OnClick
  | OnInput(onMsg) => (value => onMsg(value)->fn)->OnInput
  }
}

type property<'msg> =
  | Attribute(attribute)
  | Event(event<'msg>)

let mapProperty = (p: property<'a>, fn: 'a => 'b): property<'b> => {
  switch p {
  | Attribute(attr) => Attribute(attr)
  | Event(e) => Event(mapEvent(e, fn))
  }
}

// Attribute Creators
let id = val => Id(val)->Attribute
let className = val => ClassName(val)->Attribute

// Event Creators
let onClick = msg => OnClick(msg)->Event
let onInput = fn => OnInput(fn)->Event

let toAttributesMap = (properties: list<property<'msg>>): HashMap.String.t<string> =>
  properties->List.reduce(HashMap.String.make(~hintSize=8), (map, property) => {
    switch property {
    | Attribute(attribute) => {
        let (key, val) = switch attribute {
        | Id(val) => ("id", val)
        | ClassName(val) => ("class", val)
        | Data(key, val) => (key, val)
        }
        HashMap.String.set(map, key, val)
        map
      }
    | Event(_event) => map
    }
  })
