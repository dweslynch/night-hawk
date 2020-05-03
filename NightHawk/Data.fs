namespace NightHawk

open System
open System.Collections.Generic

module Data =
    
    type Value =
        | Str of String
        | Character of char
        | Integer of int32
        | Dbl of double
        | Bool of bool
        | Unit

    type Call =
        | Print     // Console.WriteLine()
        | PrintNL   // Console.WriteLine()
        | Add       // Add integers, concatenate strings, etc
        | Subtract
        | Multiply
        | Div
        | Mod
        | Divide    // Full division that stacks both div and mod
        | Or
        | And
        | Xor
        | Nor
        | Nand
        | Xnor
        | Not
        | Equal
        | NotEqual
        | LessThan
        | GreaterThan
        | LessThanEqual
        | GreaterThanEqual
        | Swap      // Swap top two items of stack
        | PushUnit  // Valid value
        | PushNone  // Throws exception on execution
        | PopNone
        | CharToInt
        | IntToChar
        | Store
        | Load
        //| IncrementMP
        //| DecrementMP
        //| ResetMP
        //| PushMP
        | Exit

    type LType =
        | LoadLabel of string       // Pushes the value at the label's memory address onto the stack
                                    // (if it exists).  Otherwise throws exception
        | StoreLabel of string      // Stores value from the stack at the label's location, if it exists
                                    // If not, creates the label with width 1 at next available address
        | LoadLabelOffset of string // Loads value from label+offset onto the stack (offset is top element of stack)
        | StoreLabelOffset of string
        | DefineLabel of string     // Defines (or redefines) an instruction label
        | FetchLabel of string      // Pushes label's value to the stack
        | GoToLabel of string
        | JumpAndLink of string
        | BranchIf of string
        | BranchIfEqual of string
        | BranchIfNotEqual of string
        //| BranchIfLessThan of string

    type IType =
        | PushImmediate of Value
        | Pop of int              // Pop top of stack into locals
        | Push of int             // Push local variable onto stack
        | Poll of int
        // Add more later

    type LIType =
        | Allocate of string * int              // Defines a memory label, and how much space it takes up
                                                // If not, creates the label with width 1 at next available address
        | CreateLabel of string * int

    type Instruction =
        | Call of Call
        | LType of LType
        | IType of IType
        | LIType of LIType

    type Stack () =
        let list = new List<Value option>()

        member this.Push(v : Value) =
            list.Insert(0, Some v)

        member this.Push(v : Value option) =
            list.Insert(0, v)

        member this.Access(i : int) =
            if list.Count <= i then
                IndexOutOfRangeException ("Stack is too small to access element" + i.ToString()) |> raise
            else
                let x = list.[i]
                list.RemoveAt(0)
                x

        member this.Poll() : Value option =
            if list.Count < 1 then
                IndexOutOfRangeException "Attempted access on empty stack" |> raise
            else
                list.[0]

        member this.Pop() : Value option =
            if list.Count < 1 then
                IndexOutOfRangeException "Attempted access on empty stack" |> raise
            else
                this.Access(0)