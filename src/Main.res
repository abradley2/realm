open Run
open VirtualElement
open HTMLElement
open Property


type model = {
  count: int,
  newTodoName: string,
  todos: list<string>
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

let app: application<msg, model> = {
  init,
  update: (model, msg) => switch msg {
  | Init => model
  | CountButtonClicked(value) => { ...model, count: model.count + value }
  | NewTodoNameChanged(newTodoName) => { ...model, newTodoName }
  | AddTodoButtonClicked => { ...model, newTodoName: "a", todos: list{ model.newTodoName, ...model.todos }}
  | RemoveTodoClicked(target) =>
    {
      ...model,
      todos: Belt.List.reduce(model.todos, list{}, (acc, cur) => {
        if cur == target {
          acc
        } else {
          list{cur, ...acc}
        }
      })
    }
  },
  view: model =>
   div(
    list{}, 
    list{
      div(
        list{},
        list{
          button(
            list{
              onClick(CountButtonClicked(1))
            },
            list{
              text("Click score: " ++ Belt.Int.toString(model.count))
            }
          )
        }
      ),
      div(
        list{},
        list{
          input(
            list{
              onInput((value) => NewTodoNameChanged(value))
            },
            list{}
          ),
          button(
            list{
              onClick(AddTodoButtonClicked)
            },
            list{
              text("Add todo")
            }
          ),
          ...Belt.List.map(model.todos, todo => {
            h3(
              list{
                onClick(RemoveTodoClicked(todo))
              },
              list{
                text(todo)
              }
            )
          })
        }
      )
    }
  )
}

let el = createElement("div")

let _ = body->appendChild(el->liftElement)

let _ = run(app, Init, el)
