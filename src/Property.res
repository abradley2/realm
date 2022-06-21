open Belt

type attribute =
  | Id(string)
  | ClassName(string)
  | Value(string)
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

let mapProperties = (p: list<property<'a>>, fn: 'a => 'b): list<property<'b>> =>
  p->List.map((property: property<'a>) => {
    mapProperty(property, fn)
  })

// Attribute Creators
let id = val => Id(val)->Attribute
let className = val => ClassName(val)->Attribute
let value = val => Value(val)->Attribute
let attr = (name, val) => Data(name, val)->Attribute

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
        | Value(val) => ("value", val)
        | Data(key, val) => (key, val)
        }
        HashMap.String.set(map, key, val)
        map
      }
    | Event(_event) => map
    }
  })
