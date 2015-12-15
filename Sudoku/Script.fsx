// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open Sudoku
open System

// Define your library scripting code here

type sudokuGrid = sudokuCell list
and sudokuCell = { Value:CellValue; x:int; y:int }
and CellValue = 
    | Resolved of int               //concrete value of a cell - it's either part of the sudoku grid definition, or we deduced this value
    | Options of int list   //possible solutions for the given cell

//functions to get specific ranges
let rec getRowOfCell cell sudoku =
    match sudoku with
    | [] -> []
    | head::tail ->
        match head with        
        | v when cell.y < v.y -> []
        | v when v.x = cell.x && v.y = cell.y -> getRowOfCell cell tail
        | v when v.y = cell.y -> v::getRowOfCell cell tail
        | _ -> getRowOfCell cell tail

let rec getColumnOfCell cell sudoku =
    match sudoku with
    | [] -> []
    | head::tail ->
        match head with
        | v when v.x = cell.x && v.y = cell.y -> getColumnOfCell cell tail
        | v when v.x = cell.x -> v::getColumnOfCell cell tail
        | _ -> getColumnOfCell cell tail

let rec getSudkoBoxOfCell cell sudoku =
    match sudoku with
    | [] -> []
    | head::tail ->
        match head with
        | v when v.x = cell.x && v.y = cell.y -> getSudkoBoxOfCell cell tail//the same cell
        | v when v.x >= (cell.x/3)*3 && v.x < ((cell.x/3)*3)+3 && v.y >= (cell.y/3)*3 && v.y < ((cell.y/3)*3)+3 -> v::getSudkoBoxOfCell cell tail
        | _ -> getSudkoBoxOfCell cell tail

let rec isRangeSolvedWith x (cells: sudokuCell list) : bool =
    match cells with
    | [] -> false
    | head::tail -> 
        match head.Value with
        | Resolved v -> if x = v then true else isRangeSolvedWith x tail
        | _ -> isRangeSolvedWith x tail

let rec notPosibleElseWhere x (cells: sudokuCell list) : bool =
    match cells with
    | [] -> true
    | head::tail ->
        match head.Value with
        | Options ops -> if ops |> List.forall (fun n -> x <> n) then notPosibleElseWhere x tail else false
        | _ -> notPosibleElseWhere x tail

type pos = {x:int;y:int}
let getSudokuBoxes (sudoku: sudokuGrid) =
    sudoku |> List.groupBy (fun c -> {x=c.x/3; y=c.y/3} )

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//solve functions
let solveCellRange (cell:sudokuCell) (sudoku:sudokuGrid) (range: sudokuCell list) :sudokuCell =
    match cell.Value with
    | Resolved _ -> cell
    | Options list ->         
        let newList = list |> List.filter (fun x -> not (isRangeSolvedWith x range))
        if newList |> List.length = 1 then
            {cell with Value = Resolved (newList |> List.head)}
        else
            match newList |> List.tryFind (fun x -> notPosibleElseWhere x range) with
            | Some v -> {cell with Value = Resolved v}
            | _ -> {cell with Value = Options newList}

//solve functions
let solveCellHorizontal (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
    solveCellRange cell sudoku (getRowOfCell cell sudoku)

let solveCellVertical (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
     solveCellRange cell sudoku (getColumnOfCell cell sudoku)

//a sudokubox is a 3x3 grid in the sudoku
let solveCellSudokuBox (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
    solveCellRange cell sudoku (getSudkoBoxOfCell cell sudoku)

//http://www.sudokuhints.nl/duo's
let findDuosHorizontal (sudoku:sudokuGrid) =
    let rec CheckForDuo cells box =
        match cells with
        | [] -> []
        | head::tail -> 
            match head.Value with
            | Resolved _ -> CheckForDuo tail box
            | Options ops -> 
                let row = head.y
                let rec canX x cells =
                    match cells with
                    | [] -> false
                    | head::tail ->
                        match head.Value with
                        | Resolved _ -> canX x tail
                        | Options ops -> if head.y <> row && ops |> List.exists(fun op -> x = op) then true else canX x tail

                if not (ops |> List.exists(fun op -> canX op box)) then
                    head :: CheckForDuo tail box
                else
                    CheckForDuo tail box

    sudoku |> getSudokuBoxes |> List.map snd |> List.map (fun cells -> CheckForDuo cells cells)

let findDuosVertical (sudoku:sudokuGrid) =
    let rec CheckForDuo cells box =
        match cells with
        | [] -> []
        | head::tail -> 
            match head.Value with
            | Resolved _ -> CheckForDuo tail box
            | Options ops -> 
                let col = head.x
                let rec canX x cells =
                    match cells with
                    | [] -> false
                    | head::tail ->
                        match head.Value with
                        | Resolved _ -> canX x tail
                        | Options ops -> if head.x <> col && ops |> List.exists(fun op -> x = op) then true else canX x tail

                if not (ops |> List.exists(fun op -> canX op box)) then
                    head :: CheckForDuo tail box
                else
                    CheckForDuo tail box

    sudoku |> getSudokuBoxes |> List.map snd |> List.map (fun cells -> CheckForDuo cells cells)
    

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let isSolved (sudoku:sudokuGrid) : bool =
    sudoku |> List.forall (fun c -> match c.Value with | Resolved _ -> true | _-> false)

let solveFunctions = [solveCellHorizontal;solveCellVertical;solveCellSudokuBox;]

let rec solveSudoku (sudoku:sudokuGrid) : sudokuGrid =
    let solvedSudoku = solveFunctions |> List.fold (fun sudoku f -> sudoku |> List.map (fun cell -> f cell sudoku)) sudoku
    if solvedSudoku = sudoku then
        // TODO sudoku is imposible to solve with the current algorithm
        solvedSudoku
    elif solvedSudoku |> isSolved then
        solvedSudoku
    else
        solveSudoku solvedSudoku


let rawSudoku = "062403817010006003038710002040960270087030500106074089800201700050007904274650130"

let parseRawSudoku (rs:string) : sudokuGrid =
    let getSvalue (c:char) =
        let nvalue = Char.GetNumericValue c |> int
        if nvalue < 1 then
            Options [1..9]
        else
            Resolved nvalue

    rs |> Seq.map getSvalue
        |> Seq.mapi (fun i v -> {Value = v; x = i%9; y = i/9})
        |> Seq.toList

let validateSudoku (sudoku:sudokuGrid) =
    let is45 list =
        list |> List.map (fun row -> row |> List.sumBy (fun cell -> match cell.Value with | Resolved r -> r |_->0)) |> List.forall (fun a -> a = 45)
    //get rows
    let rows = sudoku |> List.groupBy (fun c -> c.y) |> List.map snd |> is45
    let cols = sudoku |> List.groupBy (fun c -> c.y) |> List.map snd |> is45
    let boxes = sudoku |> getSudokuBoxes |> List.map snd |> is45
    rows && cols && boxes


let printSudoku (sudoku:sudokuGrid) =
    let printCell i cell =
        let v = match cell.Value with
                | Resolved var -> var
                | _-> 0
        if i % 9 = 8 then
            printfn "%i" v
        else printf "%i" v
    sudoku |> List.iteri printCell
    ()

let readSudokuFile =
    System.IO.File.ReadAllLines("E:\drive\Hro\dev2\Sudoku\Sudoku\0.txt") |> Seq.toList |> List.map parseRawSudoku 

#time
let sudokus = readSudokuFile |> List.map solveSudoku 
#time

#time
let sudokusp = readSudokuFile |> List.toArray |> Array.Parallel.map solveSudoku 
#time

sudokus |> List.countBy isSolved

sudokus |> List.filter (fun s -> (isSolved s)) |> List.map validateSudoku |> List.forall (fun b -> b)

sudokus |> List.filter (fun s -> not (isSolved s)) |> List.item 1 |> printSudoku
sudokus |> List.filter (fun s -> not (isSolved s)) |> List.item 1 |> findDuosHorizontal
sudokus |> List.filter (fun s -> not (isSolved s)) |> List.item 1 |> findDuosVertical

rawSudoku |> parseRawSudoku |> findDuosHorizontal


//1) list comprehensions
//2) list monad
//3) async monad
//4) webcam -> picture -> rgb -> hsv -> h -> count occurrences -> make histogram -> draw with windows forms

