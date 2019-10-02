module Week3


open System

let do_funcs() =
    let get_sum (x : int, y : int) : int = x+y
    printfn "5 + 7 = %i" (get_sum(5,7))

do_funcs()

Console.ReadKey() |> ignore