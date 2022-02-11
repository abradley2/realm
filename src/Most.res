type error = {message: string}

type disposable = {dispose: unit => unit}

type sink<'a> = {
  event: 'a => unit,
  error: error => unit,
  end: unit => unit,
}

type stream<'a> = {run: sink<'a> => disposable}

type loopResult<'seed, 'value> = {seed: 'seed, value: 'value}

let empty = (): stream<'never> => {
  run: (_sink: sink<'never>) => {
    dispose: () => (),
  },
}

let startWith = (stream: stream<'event>, event: 'event): stream<'event> => {
  run: (sink: sink<'event>) => {
    let _ = Js.Global.setTimeout(() => {
      sink.event(event)
    }, 0)

    stream.run(sink)
  },
}

let map = (stream: stream<'a>, fn: 'a => 'b): stream<'b> => {
  run: (sink: sink<'b>) => {
    let mapSink = {
      event: (event: 'a) => {
        let mappedEvent = fn(event)
        sink.event(mappedEvent)
      },
      error: sink.error,
      end: sink.end,
    }

    stream.run(mapSink)
  },
}

let loop = (
  stream: stream<'event>,
  fn: ('seed, 'event) => loopResult<'seed, 'value>,
  seed: 'seed,
): stream<'value> => {
  run: (sink: sink<'value>) => {
    let currentSeed = ref(seed)

    let loopSink: sink<'event> = {
      event: (event: 'event) => {
        let loopResult = fn(currentSeed.contents, event)
        currentSeed.contents = loopResult.seed

        sink.event(loopResult.value)
      },
      error: sink.error,
      end: sink.end,
    }

    stream.run(loopSink)
  },
}

let merge = (a: stream<'a>, b: stream<'a>): stream<'a> => {
  run: (sink: sink<'a>) => {
    let disposeA = a.run(sink)
    let disposeB = b.run(sink)

    {
      dispose: () => {
        disposeA.dispose()
        disposeB.dispose()
      },
    }
  },
}

let take = (stream: stream<'a>, count: int): stream<'a> => {
  let done = ref(0)
  {
    run: (sink: sink<'a>) => {
      let rec oneSink: sink<'a> = {
        event: event => {
          sink.event(event)
          done.contents = done.contents + 1
          if done.contents >= count {
            oneSink.end()
          }
        },
        error: sink.error,
        end: sink.end,
      }

      stream.run(oneSink)
    },
  }
}
