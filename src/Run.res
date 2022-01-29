open Element

type application<'msg, 'model> = {
  init: 'model,
  update: ('model, 'msg) => 'model,
  view: 'model => node<'msg>
}

exception InvalidRoot(string)

let runDomDiff = (fromEl: Dom.htmlElement, toEl: element): unit => {
  switch toEl {
  | Text(_toEl) => raise(InvalidRoot("Cannot have a text element as the view root"))
  | Element(_toEl) => Morphdom.update(fromEl, _toEl)
  }
}

let run = (app: application<'msg, 'model>, msg: 'msg, hostEl: Dom.htmlElement): Most.disposable => {
  let eventSource: ref<'msg => unit> = ref((_msg) => ())

  let applicationScheduler = Most.newDefaultScheduler()

  let eventStream: Most.stream<'msg> = {
    run: (. sink, scheduler) => {
      eventSource := (msg: 'msg): unit => {
        sink.event(. scheduler.currentTime(), msg)
      }
      {
        dispose: () => eventSource := (_msg) => ()
      }
    }
  }

  let applicationStream = 
    eventStream->Most.loop(
      (prevModel, msg) => {
        let nextModel = app.update(prevModel, msg)
        {
          seed: nextModel,
          value: app.view(nextModel)->createElement
        }
      },
      app.init
    )

  let applicationSink: Most.sink<Element.streamElement<'msg>> = {
    event: (. _time, streamElement) => {
      
      runDomDiff(hostEl, streamElement.el)

      let nextEvent = streamElement.stream->Most.take(1)

      let _ = nextEvent.run(
        .
        {
          event: (. _time, msg) => {
            eventSource.contents(msg)
          },
          end: _time => (),
          error: (. _time, err) => {
            Js.Console.error(err)
          }
        },
        applicationScheduler
      )
      ()
    },
    end: _time => (),
    error: (. _time, err) => {
      Js.Console.error(err)
    }
  }

  let _ = Js.Global.setTimeout(() => {
    eventSource.contents(msg)
  }, 0)

  Most.run(applicationSink, applicationScheduler, applicationStream)
}

