open Run
open VirtualElement
open HTMLElement
open Property

type model = {
  count: int,
  newTodoName: string,
  todos: list<string>,
}

type msg =
  | Init
  | CountButtonClicked(int)
  | NewTodoNameChanged(string)
  | AddTodoButtonClicked
  | RemoveTodoClicked(string)

let init: model = {
  count: 0,
  newTodoName: "",
  todos: list{},
}

let countButton = (label: string): vnode<int> => button(list{onClick(1)}, list{text(label)})

let countBy2Button = (model: model) =>
  countButton(
    "Increase " ++ Belt.Int.toString(model.count) ++ " by 2",
  )->mapVnode(value => CountButtonClicked(value * 2))

let countBy100Button = (model: model) =>
  countButton(
    "Increase " ++ Belt.Int.toString(model.count) ++ " by 100",
  )->mapVnode(value => CountButtonClicked(value * 100))

let app: application<msg, model> = {
  init: init,
  update: (model, msg) =>
    switch msg {
    | Init => model
    | CountButtonClicked(value) => {...model, count: model.count + value}
    | NewTodoNameChanged(newTodoName) => {...model, newTodoName: newTodoName}
    | AddTodoButtonClicked => {
        ...model,
        newTodoName: "",
        todos: list{model.newTodoName, ...model.todos},
      }
    | RemoveTodoClicked(target) => {
        ...model,
        todos: Belt.List.reduce(model.todos, list{}, (acc, cur) => {
          if cur == target {
            acc
          } else {
            list{cur, ...acc}
          }
        }),
      }
    },
  view: model =>
    div(
      list{},
      list{
        div(list{}, list{countBy2Button(model), countBy100Button(model)}),
        div_(list{
          input'(list{onInput(value => NewTodoNameChanged(value)), value(model.newTodoName)}),
          button(list{onClick(AddTodoButtonClicked)}, list{text("Add todo")}),
          ...Belt.List.map(model.todos, todo => {
            h3(list{onClick(RemoveTodoClicked(todo))}, list{text(todo)})
          }),
        }),
      },
    ),
}

let el = createElement("div")

let _ = body->appendChild(el->liftElement)

let _ = run(app, Init, el)
