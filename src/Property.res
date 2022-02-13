open Belt

type attribute =
  | Id(string)
  | ClassName(string)
  | Data(string, string)

type event<'msg> =
  | OnClick('msg)
  | OnInput(string => 'msg)

type property<'msg> =
  | Attribute(attribute)
  | Event(event<'msg>)

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
