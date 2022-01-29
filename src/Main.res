open Element
open Property
open Run
open Belt

let el = HTMLElement.createElement("div")

HTMLElement.body->HTMLElement.appendChild(el)

let myArray = [1, 2, 3, 4]

let t = (v: bool) =>
  switch v {
  | true => 1
  | false => 0
  }

let rec searchItems = (items: list<int>, target: int) =>
  switch items {
  | list{} => None
  | list{item, ...nextItems} =>
    switch item === target {
    | true => Some(item)
    | _ => searchItems(nextItems, target)
    }
  }

let b = myArray->List.fromArray->searchItems(2)

let rec findItems = (arr: array<int>, target: int) => {
  switch arr {
  | [] => None
  | items =>
    if Option.mapWithDefault(items[0], false, item => item == target) {
      Some(target)
    } else {
      findItems(Array.sliceToEnd(arr, 1), target)
    }
  }
}

Js.log(findItems(myArray, 3))

let _ = run(
  {
    init: 0,
    update: (model, msg) => model + msg,
    view: model =>
      div(
        list{className("vh-100 vw-100 bg-green blue flex flex-column items-center justify-center")},
        list{
          button(list{onClick(1)}, list{text("Clicked " ++ Js.Int.toString(model) ++ " times")}),
          switch model > 0 {
          | true => button(list{onClick(100)}, list{text("Add one hundred")})
          | false => button(list{}, list{text("Add Ten")})
          },
        },
      ),
  },
  0,
  el,
)
