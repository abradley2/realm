open Belt
open Run
open VirtualElement
open HTMLElement
open Property

let app: application<int, int> = {
  init: 0,
  update: (model, msg) => model + msg,
  view: model =>
    button(list{onClick(1)}, list{text("Clicked " ++ Int.toString(model) ++ " times")}),
}

let el = createElement("div")

setId(el, "app-host")

let _ = body->appendChild(el->liftElement)

let _ = run(app, 0, el)

let a = [1, 2, 3, 4, 5, 6, 7]->Js.Array2.reduce((acc, cur) => {
  Int.toString(cur)->HashMap.String.set(acc, _, "true")
  acc
}, HashMap.String.make(~hintSize=0))

Js.Console.log(HashMap.String.toArray(a))
