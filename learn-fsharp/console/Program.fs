// Learn more about F# at http://fsharp.org

open System
open FSharp.Json

type Foo =
  { meta : Bar }
and Bar =
  { count : int }


let json = """{"meta": {"count": 15}}"""

let data = Json.deserialize<Foo> json

let hello (x : string) =
  printfn "The value was %s" x

let h (x : string) =
  x.Length

[<EntryPoint>]
let main argv =
    printfn "%A" data
    0 // return an integer exit code

