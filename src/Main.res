open Belt

let a = [1, 2, 3, 4, 5, 6, 7]->Js.Array2.reduce((acc, cur) => {
  Int.toString(cur)->HashMap.String.set(acc, _, "true")
  acc
}, HashMap.String.make(~hintSize=0))

Js.Console.log(HashMap.String.toArray(a))
