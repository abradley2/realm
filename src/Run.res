open HTMLElement
open VirtualElement
open Most

type application<'msg, 'model> = {
  init: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => vnode<'msg>,
}

exception InvalidRoot(string)

let runDomDiff = (fromEl: Dom.htmlElement, virtualElement: virtualElement<'msg>): stream<'msg> => {
  let stream = ref(empty())
  let _ = Patch.render(Element(fromEl), virtualElement, stream)
  stream.contents
}

let _rootId = ref(0)

let getRootId = () => {
  _rootId.contents = _rootId.contents + 1
  "realm-root-element--" ++ Belt.Int.toString(_rootId.contents)
}

let run = (app: application<'msg, 'model>, msg: 'msg, hostEl: Dom.htmlElement): disposable => {
  let eventSource: ref<'msg => unit> = ref(_msg => ())

  let rootId = getRootId()

  hostEl->setAttribute(rootId, "true")

  let eventStream: stream<'msg> = {
    run: sink => {
      eventSource :=
        (
          (msg: 'msg): unit => {
            sink.event(msg)
          }
        )

      {
        dispose: () => eventSource := (_msg => ()),
      }
    },
  }->startWith(msg)

  let applicationStream = eventStream->loop((prevModel, msg) => {
    let nextModel = app.update(prevModel, msg)
    {
      seed: nextModel,
      value: app.view(nextModel)->createVirtualElement,
    }
  }, app.init)

  let applicationSink: sink<virtualElement<'msg>> = {
    event: virtualElement => {
      let renderRoot = switch querySelector(document, "[" ++ rootId ++ "]") {
      | Some(el) => el
      | None => raise(InvalidRoot("No root element found"))
      }

      let stream = runDomDiff(renderRoot, virtualElement)

      switch virtualElement.el {
        | Element(el) => el->setAttribute(rootId, "true")
        | _ => ()
      }

      let nextEvent = stream->take(1)

      let _ = nextEvent.run({
        event: msg => {
          eventSource.contents(msg)
        },
        end: () => (),
        error: err => {
          Js.Console.error(err)
        },
      })
    },
    end: _time => (),
    error: err => {
      Js.Console.error(err)
    },
  }

  applicationStream.run(applicationSink)
}
