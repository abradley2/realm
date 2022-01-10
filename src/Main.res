open Element
open Property
open Run

let el = HTMLElement.createElement("div")

HTMLElement.body->HTMLElement.appendChild(el)

let _ = run({
  init: 0,
  update: (model, msg) => model + msg,
  view: model => div(
    list{
      className("vh-100 vw-100 bg-green blue flex flex-column items-center justify-center")
    },
    list{
      button(
        list{
          onClick(1)
        },
        list{text("Clicked " ++ Js.Int.toString(model) ++ " times")}
      )
    },
  )->createElement
}, 0, el)

