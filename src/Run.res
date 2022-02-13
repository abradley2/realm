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

let run = (app: application<'msg, 'model>, msg: 'msg, hostEl: Dom.htmlElement): disposable => {
  let eventSource: ref<'msg => unit> = ref(_msg => ())

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
      value: app.view(nextModel)->createElement,
    }
  }, app.init)

  let applicationSink: sink<virtualElement<'msg>> = {
    event: virtualElement => {
      let stream = runDomDiff(hostEl, virtualElement)

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
