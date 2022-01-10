type attribute =
  | Id(string)
  | ClassName(string)

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
