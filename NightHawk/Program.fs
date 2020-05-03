namespace NightHawk

open System
open System.Collections.Generic
open Data
open Tokenizer
open System.IO

// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.

module Main =

    let memory = new List<Value> ()
    let stack  = new Stack ()
    let labels = new Dictionary<string, int> () // label: line number
                                                // label: memory subscript
    let mutable pullout = false
    let mutable line = 0

    let locals : Value option array = [|None; None; None; None; None; None; None; None|]

    let advance () =
        line <- line + 1
        ()

    let ``none error`` i =
        ArgumentNullException ("`None` access at line " + i.ToString()) |> raise

    let ``unary error`` operation a i =
        ArgumentException (operation + " operation at " + i.ToString() + " does not support " + a.ToString()) |> raise

    let ``operation error`` operation a b i =
        ArgumentException (operation + " operation at " + i.ToString() + " does not support this combination of types: " + a.ToString() + " " + b.ToString()) |> raise

    let ``label error`` label i =
        ArgumentException ("label " + label + " access at line " + i.ToString() + " does not exist") |> raise

    let ``memory bounds error`` i =
        ArgumentException ("Memory address " + i.ToString() + " is outside available memory bounds - line " + line.ToString()) |> raise
    
    let ``locvar error`` i =
        ArgumentException ("Local variable " + i.ToString() + " does not exist") |> raise

    let ``type error`` required presented =
        ArgumentException ("Type mismatch: " + required + " required, " + presented + " presented at line " + line.ToString()) |> raise

    // Prints value without newline
    // printing `unit` yields no output
    let ``evaluate Print`` value =
        match value with
        | Str s -> Console.Write s
        | Character c -> Console.Write c
        | Integer i -> Console.Write i
        | Dbl d -> Console.Write d
        | Bool b ->
            match b with
            | true -> Console.Write "True"
            | false -> Console.Write "False"
        | Unit -> ignore None

    // Adds integers (int + int) and doubles (double + double)
    // (int + double) -> int, (double + int) -> double)
    // Concatenates strings (str + str, str + char, char + str, str + int)
    // Adding (_, unit) is an identity
    // Adding (unit, _) returns unit
    // Adding (unit, unit) is a degenerate case that merges the units
    let ``evaluate Add`` ops opt =
        match ops, opt with
        | Str s, Str t ->               s + t |> Str
        | Str s, Character c ->         s + c.ToString() |> Str
        | Character c, Str s ->         c.ToString() + s |> Str
        | Character c, Character d ->   c.ToString() + d.ToString() |> Str
        | Str s, Integer i ->           s + i.ToString() |> Str
        | Str s, Dbl d ->               s + d.ToString() |> Str
        | Integer i, Integer j ->       i + j |> Integer
        | Integer i, Dbl d ->           i + int d |> Integer
        | Dbl d, Integer i ->           d + double i |> Dbl
        | x, Unit ->                    x
        | Unit, _ ->                    Unit
        | a, b ->                       ``operation error`` "Add" a b line

    // Subtracts integers and doubles according to addition rules
    // Unit rules are the same as addition
    let ``evaluate Subtract`` ops opt =
        match ops, opt with
        | Integer i, Integer j ->   i + j |> Integer
        | Dbl d, Dbl f ->           d + f |> Dbl
        | Integer i, Dbl d ->       i + int d |> Integer
        | Dbl d, Integer i ->       d + double i |> Dbl
        | x, Unit ->                x
        | Unit, _ ->                Unit
        | a, b ->                   ``operation error`` "Subtract" a b line

    // Multiplies integers and doubles according to addition/subtraction rules
    // Unit rules are the same as addition/subtraction
    let ``evaluate Multiply`` sfactor tfactor =
        match sfactor, tfactor with
        | Integer i, Integer j ->   i * j |> Integer
        | Dbl d, Dbl f ->           d * f |> Dbl
        | Integer i, Dbl d ->       i * int d |> Integer
        | Dbl d, Integer i ->       d * double i |> Dbl
        | x, Unit ->                x
        | Unit, _ ->                Unit
        | a, b ->                   ``operation error`` "Multiply" a b line

    // Precise division returning a double
    // Accepts any combination of doubles and integers
    // Unit rules are the same as other mathematical operations
    let ``evaluate Divide`` divisor dividend =
        match divisor, dividend with
            | Integer i, Integer j ->   double i / double j |> Dbl
            | Dbl d, Dbl f ->           d / f |> Dbl
            | Integer i, Dbl d ->       double i / d |> Dbl
            | Dbl d, Integer i ->       d / double i |> Dbl
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "Divide" a b line

    // Integer division
    // (int, double) takes the floor of the double
    // (double, _) throws operation exception
    // Unit identity rules apply
    let ``evaluate Div`` divisor dividend =
        match divisor, dividend with
            | Integer i, Integer j ->   i / j |> Integer
            | Integer i, Dbl d ->       i / int d |> Integer
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "Div" a b line

    // Modular division
    // Same rules apply as for `Div`
    let ``evaluate Mod`` divisor dividend =
        match divisor, dividend with
            | Integer i, Integer j ->   i % j |> Integer
            | Integer i, Dbl d ->       i % int d |> Integer
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "Div" a b line

    // Logical `or`, performed on (int, int) or (bool, bool)
    // Unit identity rules apply
    let ``evaluate Or`` ops opt =
        match ops, opt with
            | Integer i, Integer j ->   i ||| j |> Integer
            | Bool b, Bool c ->         (b || c) |> Bool
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "Or" a b line

    // Logical `and`
    // Same rules apply as for `or`
    let ``evaluate And`` ops opt =
        match ops, opt with
            | Integer i, Integer j ->   i &&& j |> Integer
            | Bool b, Bool c ->         (b && c) |> Bool
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "And" a b line

    // Logical `xor`
    // Same rules apply as for `and` and `or`
    let ``evaluate Xor`` ops opt =
        match ops, opt with
            | Integer i, Integer j ->   i ^^^ j |> Integer
            | Bool b, Bool c ->         ((b || c) && (not(b && c))) |> Bool
            | x, Unit ->                x
            | Unit, _ ->                Unit
            | a, b ->                   ``operation error`` "Xor" a b line

    // Logical `not` for int and bool
    // Unit -> Unit
    let ``evaluate Not`` operand =
        match operand with
            | Integer i ->  ~~~i |> Integer
            | Bool b ->     (not b) |> Bool
            | Unit ->       Unit
            | a ->          ``unary error`` "Not" a line

    // Tests direct equality
    // (unit, unit) and (_, unit) -> True
    // (unit, _) -> False
    // (_, _) -> False
    let ``evaluate Equal`` ops opt =
        match ops, opt with
            | Str s, Str t ->               s.Equals(t) |> Bool
            | Character _c, Character _d -> (_c = _d) |> Bool
            | Integer i, Integer j ->       (i = j) |> Bool
            | Dbl d, Dbl f ->               (d = f) |> Bool
            | Bool b, Bool c ->             (b = c) |> Bool
            | _, Unit ->                    Bool true
            | Unit, _ ->                    Bool false
            | _, _ ->                       Bool false

    // Compares two values
    // unit rules are the same as for `Equal`
    // All illegal values throw exception
    let ``evaluate Less Than`` ops opt =
        match ops, opt with
            | Character _c, Character _d -> ((int _c) < (int _d)) |> Bool
            | Integer i, Integer j ->       (i < j) |> Bool
            | Dbl d, Dbl f ->               (d < f) |> Bool
            | Bool b, Bool c ->             (b < c) |> Bool
            | _, Unit ->                    Bool true
            | Unit, _ ->                    Bool false
            | a, b ->                       ``operation error`` "Less Than" a b line

    // Compares two values
    // Rules are identical to Less Than
    let ``evaluate Greater Than`` ops opt =
        match ops, opt with
            | Character _c, Character _d -> ((int _c) > (int _d)) |> Bool
            | Integer i, Integer j ->       (i > j) |> Bool
            | Dbl d, Dbl f ->               (d > f) |> Bool
            | Bool b, Bool c ->             (b > c) |> Bool
            | _, Unit ->                    Bool true
            | Unit, _ ->                    Bool false
            | a, b ->                       ``operation error`` "Greater Than" a b line

    // Converts character to its ASCII code
    // Unit -> 0
    let ``evaluate CharToInt`` character =
        match character with
            | Character c ->    int c |> Integer
            | Unit ->           Integer 0

    // Converts integer to ASCII character
    // Unit -> Unit
    let ``evaluate IntToChar`` integer =
        match integer with
            | Integer i ->  char i |> Character
            | Unit ->       Unit
            

    let rec execute qadvance = function
        | Call c ->
            match c with
                | Exit -> pullout <- true
                | Print ->
                    match stack.Pop() with
                        | Some value ->
                            ``evaluate Print`` value
                        | None -> ``none error`` line
                    if qadvance then advance()          // Allows pseudoinstructions to execute without advancing
                | PrintNL ->
                    match stack.Pop() with
                        | Some value ->
                            ``evaluate Print`` value
                            Console.WriteLine()
                        | None -> ``none error`` line
                    if qadvance then advance()
                | Add ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Add`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | Subtract ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Subtract`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | Multiply ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Multiply`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | Divide ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Divide`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | Or ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Or`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | And ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate And`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | Xor ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Xor`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                // Logical `nor` on (int, int) or (bool, bool)
                // (unit, _) yields unit
                // (_, unit) INVERTS value
                | Nor ->
                    Or |> Call |> execute false     // Execute `or` without advancing counter
                    Not |> Call |> execute false
                    if qadvance then advance()
                // Logical `nand` follows same rules as nor
                | Nand ->
                    And |> Call |> execute false
                    Not |> Call |> execute false
                    if qadvance then advance()
                // Logical `xnor` follows same rules as nor/nand
                | Xnor ->
                    Xor |> Call |> execute false
                    Not |> Call |> execute false
                    if qadvance then advance()
                | Equal ->
                    match stack.Pop(), stack.Pop() with
                        | Some ops, Some opt ->
                            let result = ``evaluate Equal`` ops opt
                            stack.Push result
                        | _, _ -> ``none error`` line
                    if qadvance then advance()
                | LessThan ->
                    match stack.Pop(), stack.Pop() with
                    | Some ops, Some opt ->
                        let result = ``evaluate Less Than`` ops opt
                        stack.Push result
                    | _, _ -> ``none error`` line
                    if qadvance then advance()
                | GreaterThan ->
                    match stack.Pop(), stack.Pop() with
                    | Some ops, Some opt ->
                        let result = ``evaluate Greater Than`` ops opt
                        stack.Push result
                    | _, _ -> ``none error`` line
                    if qadvance then advance()
                // Computes less-than-or-equal for two values
                // Unit rules are the same as other comparisons
                | LessThanEqual ->
                    Poll 7 |> IType |> execute false
                    Poll 6 |> IType |> execute false
                    LessThan |> Call |> execute false
                    Push 6 |> IType |> execute false
                    Push 7 |> IType |> execute false
                    Equal |> Call |> execute false
                    Or |> Call |> execute false
                    if qadvance then advance()
                // Greater-than-or-equal
                // Unit rules are the same as for other comparisons
                | GreaterThanEqual ->
                    Poll 7 |> IType |> execute false
                    Poll 6 |> IType |> execute false
                    GreaterThan |> Call |> execute false
                    Push 6 |> IType |> execute false
                    Push 7 |> IType |> execute false
                    Equal |> Call |> execute false
                    Or |> Call |> execute false
                    if qadvance then advance()
                // Exact inversion of the equality operation
                // True -> False, False -> True
                // (_, unit) -> False
                // (unit, _) -> True
                | NotEqual ->
                    Equal |> Call |> execute false
                    Not |> Call |> execute false
                // Direct swap of stack elements, even if `None`
                | Swap ->
                    let top = stack.Pop()
                    let next = stack.Pop()
                    stack.Push top
                    stack.Push next
                    if qadvance then advance()
                // Push unit value onto stack
                | PushUnit ->
                    stack.Push Unit
                    if qadvance then advance()
                // Push `None` onto stack
                // Next operation will throw exception if not handled properly
                | PushNone ->
                    stack.Push None
                    if qadvance then advance()
                // Removes and ignores the top of the stack
                // Will handle `None` values
                | PopNone ->
                    stack.Pop() |> ignore
                    if qadvance then advance()
                // Convert ascii character to int
                | CharToInt ->
                    match stack.Pop() with
                        | Some character ->
                            let result = ``evaluate CharToInt`` character
                            stack.Push result
                        | None -> ``none error`` line
                    if qadvance then advance()
                // Convert int to ascii character
                | IntToChar ->
                    match stack.Pop() with
                        | Some integer ->
                            let result = ``evaluate IntToChar`` integer
                            stack.Push result
                        | None -> ``none error`` line
                    if qadvance then advance()
                // Stores the second-to-top stack value at the address indicated by the top stack value
                // Unit is ignored but all other non-integer values raise exception
                | Store ->
                    match stack.Pop() with
                        | Some value ->
                            match value with
                                | Integer addr ->
                                    if addr >= memory.Count then
                                        ``memory bounds error`` addr
                                    else
                                        match stack.Pop() with
                                            | Some item ->
                                                memory.[addr] <- item
                                            | None -> ``none error`` line
                                | Unit -> ()
                                | v -> ``type error`` "Integer" (v.ToString())
                        | None -> ``none error`` line
                    if qadvance then advance()
                // Pushes to the stack the value at the memory address indicated by the top of the stack
                // Same rules as Store apply
                | Load ->
                    match stack.Pop() with
                        | Some value ->
                            match value with
                                | Integer addr ->
                                    if addr >= memory.Count then
                                        ``memory bounds error`` addr
                                    else
                                        stack.Push memory.[addr]
                                | Unit -> ()
                                | v -> ``type error`` "Integer" (v.ToString())
                        | None -> ``none error`` line
                    if qadvance then advance()

                // Add the rest of the calls

        | LType l ->
            match l with
                // Pushes the value at `label` in memory onto the stack
                | LoadLabel label ->
                    if labels.ContainsKey label then
                        let index = labels.[label]
                        if index >= memory.Count then
                            ``memory bounds error`` index
                        stack.Push memory.[index]
                    else
                        ``label error`` label line
                    if qadvance then advance()
                // Stores the top value from the stack in memory at `label`
                | StoreLabel label ->
                    if labels.ContainsKey label then
                        let index = labels.[label]
                        if index >= memory.Count then
                            ``memory bounds error`` index
                        match stack.Pop() with
                            | Some value -> memory.[index] <- value
                            | None -> ``none error`` line
                    else
                        match stack.Pop() with
                            | Some value ->
                                labels.Add(label, memory.Count)
                                memory.Add(value)
                            | None -> ``none error`` line
                    if qadvance then advance()
                // Loads the value at the label + offset onto the stack
                // Offset is top element of stack
                | LoadLabelOffset label ->
                    FetchLabel label |> LType |> execute false  // Stack label value
                    Swap |> Call |> execute false               // Swap to preserve unit identity
                    Add |> Call |> execute false                // label + offset
                    Load |> Call |> execute false
                    if qadvance then advance()
                // Stores item second-in-stack at address (label + top-of-stack)
                | StoreLabelOffset label ->
                    FetchLabel label |> LType |> execute false
                    Swap |> Call |> execute false
                    Add |> Call |> execute false
                    Store |> Call |> execute false
                    if qadvance then advance()
                // Defines a new label in the program instructions (not memory)
                | DefineLabel label ->
                    if labels.ContainsKey label then
                        labels.[label] <- line
                    else
                        labels.Add(label, line)
                    if qadvance then advance()
                // Pushes the numeric value of the label (memory or instruction) onto the stack
                | FetchLabel label ->
                    if labels.ContainsKey label then
                        labels.[label] |> Integer |> stack.Push
                    else
                        ``label error`` label line
                    if qadvance then advance()
                // Unconditionally jumps to the label specified
                | GoToLabel label ->
                    if labels.ContainsKey label then
                        line <- labels.[label]
                    else
                        ``label error`` label line
                // Jumps to specified label and pushes current line + 1 onto stack
                | JumpAndLink label ->
                    if not (labels.ContainsKey label) then
                        ``label error`` label line

                    // Push `line` + 1 onto stack
                    line + 1|> Integer |> PushImmediate |> IType |> execute false
                    // Jump
                    GoToLabel label |> LType |> execute false
                // Branches to specified label if top-of-stack value is True
                | BranchIf label ->
                    if not (labels.ContainsKey label) then
                        ``label error`` label line

                    match stack.Pop() with
                        | Some value ->
                            match value with
                                | Bool b ->
                                    if b then
                                        GoToLabel label |> LType |> execute false
                                    else
                                        if qadvance then advance()
                                | _ ->
                                    if qadvance then advance()
                        | None ->
                            ``none error`` line
                            if qadvance then advance()
                // Branches to specified label if top two stack values are equal
                | BranchIfEqual label ->
                    if not (labels.ContainsKey label) then
                        ``label error`` label line

                    Equal |> Call |> execute false
                    BranchIf label |> LType |> execute true
                // Branches to specified label if top two stack values are not equal
                | BranchIfNotEqual label ->
                    if not (labels.ContainsKey label) then
                        ``label error`` label line

                    NotEqual |> Call |> execute false
                    BranchIf label |> LType |> execute true
                | BranchIfLessThan label ->
                    if not (labels.ContainsKey label) then
                        ``label error`` label line

                    LessThan |> Call |> execute false
                    BranchIf label |> LType |> execute true
        | IType i ->
            match i with
                // Pushes an immediate value to the stack
                // Does not accept `None`
                | PushImmediate value ->
                    stack.Push value
                    if qadvance then advance()
                // Pops a value from the stack into the local variable represented by the immediate integer
                | Pop loc ->
                    if loc < 8 then
                        match stack.Pop() with
                            | Some value -> locals.[loc] <- Some value
                            | None -> ``none error`` line
                    else
                        ``locvar error`` loc

                    if qadvance then advance()
                // Polls a value from the stack (does not remove)
                // Same rules as Pop
                | Poll loc ->
                    if loc < 8 then
                        match stack.Poll() with
                            | Some value -> locals.[loc] <- Some value
                            | None -> ``none error`` line
                    else
                        ``locvar error`` loc
                    if qadvance then advance()
                // Pushes the value of a local variable onto the stack
                // Same rules as Pop
                | Push loc ->
                    if loc < 8 then
                        stack.Push locals.[loc]
                    else ``locvar error`` loc

                    if qadvance then advance()
        | LIType li ->
            match li with
                // Allocates space in memory for corresponding label
                // If label already exists, redefine it
                | Allocate (label, width) ->
                    if labels.ContainsKey label then
                        labels.[label] <- memory.Count
                    else
                        labels.Add(label, memory.Count)

                    for x = 0 to width do
                        memory.Add(Unit)

                    if qadvance then advance()
                // Creates label with exact value if it does not exist, updates it if it does
                | CreateLabel (label, value) ->
                    if labels.ContainsKey label then
                        labels.[label] <- value
                    else labels.Add(label, value)

                    if qadvance then advance()

    [<EntryPoint>]
    let main argv =

        let instructions = 
            if argv.Length > 0 then
                let filename = argv.[0]
                File.ReadAllLines filename
                |> Array.map tokenize
            else
                let mutable lines = []
                let mutable input = Console.ReadLine()
                while not (input.Equals "-30-") do
                    lines <- input :: lines
                    input <- Console.ReadLine()
                lines
                |> List.rev
                |> List.map tokenize
                |> Array.ofList

        // Pre-process and define labels
        for i in [0 .. Array.length instructions - 1] do
            match instructions.[i] with
                | LType (DefineLabel label) ->
                    if labels.ContainsKey label then
                        () // Do nothing
                    else
                        labels.Add(label, i)
                | _ -> () // Do nothing

        while not pullout do
            instructions.[line] |> execute true

        Console.WriteLine("Execution Finished.  Press any key to continue...")
        Console.ReadKey() |> ignore

        0 // return an integer exit code