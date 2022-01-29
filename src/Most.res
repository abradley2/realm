type time = int

type offset = int

type delay = int

type period = int

type error = {message: string}

type disposable = {dispose: unit => unit}

type task = {
  run: time => unit,
  error: (. time, error) => unit,
  dispose: unit => unit,
}

type scheduledTask = {
  task: task,
  run: unit => unit,
  error: error => unit,
}

type rec scheduler = {
  currentTime: unit => time,
  scheduleTask: (. ~offset: offset, ~delay: delay, ~period: period, ~task: task) => scheduledTask,
  relative: offset => scheduler,
  cancel: scheduledTask => unit,
}

let createScheduler = (): scheduler => {
  
  let tasks = ref([])
  let rec me = {
    cancel: (t: scheduledTask) => (),
    relative: (offset: offset) => {...me, currentTime: () => me.currentTime() + offset},
    currentTime: () => Js.Date.now()->Js.Math.floor_int,
    scheduleTask: (
      . ~offset: offset,
      ~delay: delay,
      ~period: period,
      ~task: task,
    ): scheduledTask => {
      {
        task: task,
        run: () => {
          let _ = Js.Global.setTimeout(() => {
            task.run(me.currentTime())
          }, delay)
        },
        error: (err: error) => (),
      }
    },
  }

  me
}

type sink<'a> = {
  event: (. time, 'a) => unit,
  error: (. time, error) => unit,
  end: time => unit,
}

type stream<'a> = {run: (. sink<'a>, scheduler) => disposable}

type loopResult<'seed, 'value> = {seed: 'seed, value: 'value}

// Running
@module("@most/core")
external runEffects: (stream<'a>, scheduler) => Js.promise<unit, unit> = "runEffects"

@module("@most/core")
external run: (sink<'a>, scheduler, stream<'a>) => disposable = "run"

// Construction
@module("@most/core")
external empty: unit => stream<'never> = "empty"

@module("@most/core")
external now: 'a => stream<'a> = "now"

@module("@most/core")
external _fromPromise: Js.Promise.t<'a> => stream<'a> = "fromPromise"
let fromPromise = (promise: Js.Promise.t<'a>): stream<Belt.Result.t<'a, Js.Promise.error>> => {
  promise
  ->Js.Promise.then_(res => Js.Promise.resolve(Belt.Result.Ok(res)), _)
  ->Js.Promise.catch(err => Js.Promise.resolve(Belt.Result.Error(err)), _)
  ->_fromPromise
}

@module("@most/core")
external periodic: period => stream<unit> = "periodic"

// Transformation
@module("@most/core")
external _startWith: ('a, stream<'a>) => stream<'a> = "startWith"
let startWith = (stream, val) => _startWith(val, stream)

@module("@most/core")
external _map: ('a => 'b, stream<'a>) => stream<'b> = "map"
let map = (fn, stream) => _map(stream, fn)

@module("@most/core")
external _loop: (('b, 'a) => loopResult<'b, 'c>, 'b, stream<'a>) => stream<'c> = "loop"
let loop = (stream, fn, seed) => _loop(fn, seed, stream)

@module("@most/core")
external _scan: (('b, 'a) => 'b, 'b, stream<'a>) => stream<'b> = "scan"
let scan = (stream, fn, seed) => _scan(fn, seed, stream)

// Flattening
@module("@most/core")
external switchLatest: stream<stream<'a>> => stream<'a> = "switchLatest"

@module("@most/core")
external join: stream<stream<'a>> => stream<'a> = "join"

@module("@most/core")
external _chain: ('a => stream<'b>, stream<'a>) => stream<'b> = "chain"
let chain = (stream, fn) => _chain(fn, stream)

@module("@most/core")
external _concatMap: ('a => stream<'b>, stream<'a>) => stream<'b> = "concatMap"
let concatMap = (stream, fn) => _concatMap(fn, stream)

// Merging
@module("@most/core")
external merge: (stream<'a>, stream<'a>) => stream<'a> = "merge"

@module("@most/core")
external mergeArray: array<stream<'a>> => stream<'a> = "mergeArray"

@module("@most/core")
external combine: (('a, 'b) => 'c, stream<'a>, stream<'b>) => stream<'c> = "combine"

@module("@most/core")
external _combineArray: (@variadic array<'a> => 'b, array<stream<'a>>) => stream<'b> =
  "combineArray"

let combineArray = (streams: array<stream<'a>>, fn: array<'a> => 'b) => _combineArray(fn, streams)

// Slicing
@module("@most/core")
external _take: (int, stream<'a>) => stream<'a> = "take"
let take = (stream, val) => _take(val, stream)

// // Scheduler
// @module("@most/scheduler")
// external _newDefaultScheduler: unit => scheduler = "newDefaultScheduler"

let newDefaultScheduler = () => createScheduler()
