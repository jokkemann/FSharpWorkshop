// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Square =
    | Mine
    | Number of int
    | Empty

let printSquare sq = 
        match sq with
        | Square.Empty -> "Empty"
        | Square.Mine -> "Boom"
        | Square.Number i -> i.ToString()

let isOnMineCord mine ourC =
    let row = fst mine
    let column = snd mine
    let mrow = fst ourC 
    let mcolumn = snd ourC 
    if(row=mrow&&column=mcolumn) then 1
    else 0

let mineNumber ourC = 
    isOnMineCord ((fst ourC) - 1, (snd ourC) - 1) ourC +
    isOnMineCord ((fst ourC) - 1, (snd ourC)) ourC +
    isOnMineCord ((fst ourC) - 1, (snd ourC) + 1) ourC +
    isOnMineCord ((fst ourC), (snd ourC) - 1) ourC +
    isOnMineCord ((fst ourC), (snd ourC) + 1) ourC +
    isOnMineCord ((fst ourC) + 1, (snd ourC) - 1) ourC +
    isOnMineCord ((fst ourC) + 1, (snd ourC)) ourC +
    isOnMineCord ((fst ourC) + 1, (snd ourC) + 1) ourC 

let rec checkMines list =
    match list with
    | [] -> 0
    | x :: xs -> (isOnMineCord x) + checkMines xs

let generateSquare mines ourCordinate = 
    match (isOnMineCord ourCordinate), (mineNumber ourCordinate) with 
    | 1, _ ->  Square.Mine
    | 0, 0 -> Square.Empty
    | 0, x ->  Square.Number(x)

let generateRow mines =
    [generateSquare mines (0,0);generateSquare mines (0,1)]

let createGameTable mines rowLines = 
    [generateRow mines; generateRow mines; generateRow mines]



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let mines = [(0,1);(0,5);(1,2);(1,7);(2,4);(2,6)]
    //let mines = [1;3;7;]
    let gameTable = [Square.Number(1); Square.Mine; Square.Number(2); Square.Mine; Square.Empty; Square.Empty; Square.Empty; Square.Empty; Square.Empty; Square.Empty]
    let guess x = sprintf "The square is %s" (printSquare gameTable.[x])
    printfn "Result: %s" (guess 1)
    
    let y = System.Console.ReadLine()
    0