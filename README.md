## Realm

Elm Architecture in ReScript

⚠️ This is still highly experimental and largely a work in progress.
_Most_ events and attributes the DOM can expressed aren't defined yet, and there's still 
some possible edge cases out there where the DOM patch algorithm misses something.
There's also no support for `effect`s yet.
⚠️

Unlike `rescript-react`, the Virtual DOM for this library is written entirely in ReScript with the minimum JS-interop required.

## Basic Examples

Elements are defined by a DSL that mirrors that DOM. The output is known as a `vnode` type.

```example.res
button(
    list{
        onClick("Hello world!"),
        id("my-button"),
    }
    list{
        text("Hello world!"),
    }
)
```

`vnode`s may omit an "attribute list" with the `_` suffix
```example.res
div_(
    list{
        text("Hello world!")
    }
)
```


"Empty `vnode`s" should be represented as blank text nodes
```example.res
switch showNode {
| true => div_(
    list{text("Hello there")}
)
| false => text("")
}
```

A typical application defines `init`, `msg`, `model`, `update` and `view`

```example.res
type model = {
    count: int
}

type msg =
    | Init
    | IncrementBy(int)

let app: application<msg, model> = {
  init: { count: 0 },
  update: (msg, model) => switch msg {
  | Init => model
  | IncrementBy(value) => { ...model, count: model.count + value }
  },
  view: model => div_(
    list{
        button(
            list{IncrementBy(1)->onClick},
            list{text("Increase " ++ Int.toString(model.count) ++ " by one")}
        ),
        button(
            list{IncrementBy(2)->onClick},
            list{text("Increase " ++ Int.toString(model.count) ++ " by two")}
        )
    }
  )
}
```

`vnodes` are functors and can be mapped. This is the main way to scale up an application

```example.res
let countButton = (label: string): vnode<int> => button(list{onClick(1)}, list{text(label)})

let countBy2Button = (model: model) =>
  countButton(
    "Increase " ++ Belt.Int.toString(model.count) ++ " by 2",
  )->mapVnode(value => CountButtonClicked(value * 2))

let countBy100Button = (model: model) =>
  countButton(
    "Increase " ++ Belt.Int.toString(model.count) ++ " by 100",
  )->mapVnode(value => CountButtonClicked(value * 100))
```
