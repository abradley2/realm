open HTMLElement
open Element

type application<'msg, 'model> = {
  init: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => vnode<'msg>,
}

exception InvalidRoot(string)

let runDomDiff = (fromEl: Dom.htmlElement, toEl: node): unit => {
  switch toEl {
  | Text(_toEl) => raise(InvalidRoot("Cannot have a text element as the view root"))
  | Element(_toEl) => Morphdom.update(fromEl, _toEl)
  }
}

let run = (app: application<'msg, 'model>, msg: 'msg, hostEl: Dom.htmlElement): Most.disposable => {
  let eventSource: ref<'msg => unit> = ref(_msg => ())

  let eventStream: Most.stream<'msg> = {
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
  }->Most.startWith(msg)

  let applicationStream = eventStream->Most.loop((prevModel, msg) => {
    let nextModel = app.update(prevModel, msg)
    {
      seed: nextModel,
      value: app.view(nextModel)->createElement,
    }
  }, app.init)

  let applicationSink: Most.sink<Element.streamElement<'msg>> = {
    event: streamElement => {
      runDomDiff(hostEl, streamElement.el)

      let nextEvent = streamElement.stream->Most.take(1)

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
