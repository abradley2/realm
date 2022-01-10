open Belt
open Property

let applyEvent = (ev: event<'msg>, target: Dom.htmlElement): Most.stream<'msg> => {
  run: (. sink, scheduler) => {
    let (handleEvent, eventName) = switch ev {
    | OnClick(msg) => (
        (_e: Dom.event) => {
          sink.event(. scheduler.currentTime(), msg)
        },
        "click",
      )
    | OnInput(fn) => (
        (e: Dom.event) => {
          let value =
            e
            ->HTMLElement.getTarget
            ->HTMLElement.isInput
            ->Option.mapWithDefault("", HTMLElement.getValue)
          sink.event(. scheduler.currentTime(), fn(value))
        },
        "input",
      )
    }
    {
      dispose: target->HTMLElement.addEventListener(eventName, handleEvent),
    }
  },
}
