let toNumbers (text :string) =
    text
    |>Array.ofSeq
    |>Array.map(char >> int)
    |>Array.map(fun x -> match x with
                         | x when x = 32 -> 32
                         | _ -> x - 97)
    
let toCharacters(numbers:int []) =
    numbers
    |>Array.map(fun x -> match x with
                         |x when x = 32 -> 32
                         |_ -> x + 97)
    |>Array.map(int >> char)
    |> (fun chars -> new string(chars))

let shift (numbers : int [], offset : int) =
    numbers
    |>Array.map(fun x -> match x with
                         | x when x = 32 -> 32
                         |_ -> (x + offset) % 26 )
let solution (cyphertext : string, offset:int) =
   toCharacters(shift(toNumbers(cyphertext), offset)) 

let solve (cyphertext : string, numbers:int []) =
    numbers
    |>Array.map(fun x -> solution(cyphertext, x))

[<EntryPoint>]
let main argv = 
    let cypher = "A zsnw kmuuwkkxmddq kgdnwv lzw XanwLzajlqWayzl Javvdwj"
    let cyphertext = cypher.ToLower();
    let plain = toCharacters(shift(toNumbers(cyphertext), 25))

    let offsets = [|1..25|]
    let results = solve(cyphertext, offsets)
    0 