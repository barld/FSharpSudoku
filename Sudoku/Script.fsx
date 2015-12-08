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

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//solve functions
let solveCellHorizontal (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
    match cell.Value with
    | Resolved _ -> cell
    | Options list ->         
        let rowOfCell = getRowOfCell cell sudoku
        let newList = list |> List.filter (fun x -> not (isRangeSolvedWith x rowOfCell))
        if newList |> List.length = 1 then
            {cell with Value = Resolved (newList |> List.head)}
        else
            match newList |> List.tryFind (fun x -> notPosibleElseWhere x rowOfCell) with
            | Some v -> {cell with Value = Resolved v}
            | _ -> {cell with Value = Options newList}

let solveCellVertical (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
    match cell.Value with
    | Resolved _ -> cell
    | Options list ->
        let columnOfCell = getColumnOfCell cell sudoku
        let newList = list |> List.filter (fun x -> not (isRangeSolvedWith x columnOfCell))
        if newList |> List.length = 1 then
            {cell with Value = Resolved (newList |> List.head)}
        else
            match newList |> List.tryFind (fun x -> notPosibleElseWhere x columnOfCell) with
            | Some v -> {cell with Value = Resolved v}
            | _ -> {cell with Value = Options newList}

//a sudokubox is a 3x3 grid in the sudoku
let solveCellSudokuBox (cell:sudokuCell) (sudoku:sudokuGrid) :sudokuCell =
    match cell.Value with
    | Resolved _ -> cell
    | Options list ->
        let boxOfCell = getSudkoBoxOfCell cell sudoku
        let newList = list |> List.filter (fun x -> not (isRangeSolvedWith x boxOfCell))
        if newList |> List.length = 1 then
            {cell with Value = Resolved (newList |> List.head)}
        else
            match newList |> List.tryFind (fun x -> notPosibleElseWhere x boxOfCell) with
            | Some v -> {cell with Value = Resolved v}
            | _ -> {cell with Value = Options newList}


type pos = {x:int;y:int}
//http://www.sudokuhints.nl/duo's
let findDuosHorizontal (sudoku:sudokuGrid) : (pos * sudokuCell list) list =
    sudoku |> List.groupBy (fun c -> {x=c.x/3; y=c.y/3} )

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let isSolved (sudoku:sudokuGrid) : bool =
    sudoku |> List.forall (fun c -> match c.Value with | Resolved _ -> true | _-> false)

let rec solveSudoku (sudoku:sudokuGrid) : sudokuGrid =
    let solvedSudoku = 
        sudoku 
        |> List.map (fun cell -> solveCellHorizontal cell sudoku)
        |> List.map (fun cell -> solveCellVertical cell sudoku)
        |> List.map (fun cell -> solveCellSudokuBox cell sudoku)
    if solvedSudoku = sudoku then
        // TODO sudoku is imposible to solve with the curront algorithm
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

let sudokus = readSudokuFile |> List.map solveSudoku

sudokus |> List.countBy isSolved

sudokus |> List.filter (fun s -> not (isSolved s))

rawSudoku |> parseRawSudoku |> findDuosHorizontal

//let sudoku = rawSudoku |> parseRawSudoku
//
//printSudoku sudoku
//
//printfn "----"
//
//sudoku |> solveSudoku |> printSudoku


