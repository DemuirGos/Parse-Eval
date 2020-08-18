// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions
type node<'a>= 
    | Leaf 
    | Root of value:'a * children:node<'a> list
    | Node of value:'a * parent:node<'a> *children:node<'a> list
type edge<'a> =
    | EmptyEdge
    | Edge of (float->float->float)*left:node<'a>*right:node<'a>
let operator = 
    let unaryOperator:(string*(float->float->float))List = [("^",( ** ));("*",(*));("/",(/));("+",(+));("-",(-))]
    unaryOperator |> Map.ofList
let readLine = Console.ReadLine().Replace(" ","")
let wrap s = "(" + s + ")"
let ToString c = c.ToString()
let peel (s : string) = 
    let mutable tree = Root (s,[])
    let (|Number|Variable|Operator|Space|OpenBrakets|CloseBrakets|None|) input = 
        if Regex.IsMatch(input,"[0-9\.]") then Number
        elif Regex.IsMatch(input,"[*-+\/]\^") then Operator
        elif Regex.IsMatch(input,"\(") then OpenBrakets
        elif Regex.IsMatch(input,"\)") then CloseBrakets
        elif Regex.IsMatch(input,"[a-zA-Z]+") then Variable
        elif Regex.IsMatch(input," ") then Space
        else None
    let getParent (n:node<'a>)=
        match n with 
        | Node (_,parent,_) -> parent
        | Root (_,_) -> n
    let insert root element=
        match element,root with 
        | Node(_,_,_),Node (value,parent,children) -> Node(value,parent,element::children)
        | _ -> root
    let rec parse (currNode:node<'a>,currentString,i) (s : string) = 
        let currentChar =  s.Chars i|>string
        match currentChar with 
        | Number | Variable -> parse (currNode,currentString + currentChar,i+1) s
        | Operator -> ValueSome
        | OpenBrakets -> 
            let newNode = Node("",currNode,[]) |> insert currNode
            parse (newNode,"",i+1) s
        | CloseBrakets -> parse (getParent currNode,"",i+1 ) s
        | _ -> parse (currNode,currentString,i+1) s
    ""
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
