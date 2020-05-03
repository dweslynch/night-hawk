namespace NightHawk

open System
open System.Text.RegularExpressions

open Data

module Tokenizer =

    let private calls = Map.ofList [
        @"\bprint\b",   Call Print;
        @"\bprintnl\b", Call PrintNL;
        @"\badd\b",     Call Add;
        @"\bsub\b",     Call Subtract;
        @"\bmult\b",    Call Multiply;
        @"\bdivide\b",  Call Divide;
        @"\bdiv\b",     Call Div;
        @"\bmod\b",     Call Mod;
        @"\bor\b",      Call Or;
        @"\band\b",     Call And;
        @"\bxor\b",     Call Xor;
        @"\bnor\b",     Call Nor;
        @"\bnand\b",    Call Nand;
        @"\bxnor\b",    Call Xnor;
        @"\bnot\b",     Call Not;
        @"\beq\b",      Call Equal;
        @"\bnoteq\b",   Call NotEqual;
        @"\blt\b",      Call LessThan;
        @"\bgt\b",      Call GreaterThan;
        @"\blte",       Call LessThanEqual;
        @"\bgte",       Call GreaterThanEqual;
        @"\bswap\b",    Call Swap;
        @"\bpunit\b",   Call PushUnit;
        @"\bpnone\b",   Call PushNone;
        @"\bignore\b",  Call PopNone;
        @"\bcti\b",     Call CharToInt;
        @"\bitc\b",     Call IntToChar;
        @"\bstore\b",   Call Store;
        @"\bload\b",    Call Load;
        @"\bexit\b",    Call Exit;
        @"\bimp\b",     Call IncrementMP;
        @"\bdmp\b",     Call DecrementMP;
        @"\brmp\b",     Call ResetMP;
        @"\bpmp\b",     Call PushMP;
        @"\bjump\b",    Call Jump;
    ]

    let private ltypes = Map.ofList [
        "llb",      LType << LoadLabel;
        "slb",      LType << StoreLabel;
        "llbo",     LType << LoadLabelOffset;
        "slbo",     LType << StoreLabelOffset;
        "def",      LType << DefineLabel;
        "fetch",    LType << FetchLabel;
        "goto",     LType << GoToLabel;
        "jal",      LType << JumpAndLink;
        "bif",      LType << BranchIf;
        "beq",      LType << BranchIfEqual;
        "bne",      LType << BranchIfNotEqual;
        "blt",      LType << BranchIfLessThan;
    ]

    let private isNumeric (str : string) =
        let mutable numeric = true
        let numbers = "1234567890"
        for c in str do
            if not (numbers.Contains (c.ToString())) then
                numeric <- false
        numeric


    let private regexLType opcode = @"\b(" + opcode + @")\s+([a-zA-Z]+[0-9]*[a-zA-Z]*)"
    
    let tokenize line =
        // Look for a regex that matches the input
        let call = calls |> Map.tryPick (fun key value ->
            if Regex.Match(line, key).Success then Some value else None
        )

        let ltype = ltypes |> Map.tryPick (fun key value ->
            let _match = Regex.Match(line, regexLType key)
            if _match.Success then
                _match.Groups.[2].Value |> value |> Some
            else None        
        )

        // Only one ValType at the moment
        let valtype =
            let _match = Regex.Match(line, """(\bpi)\s+("[^"\n]*"|'.'|[a-zA-Z]+|[0-9]*\.[0-9]+|[0-9]+)""")
            if _match.Success then
                let imm = _match.Groups.[2].Value
                if imm.StartsWith "\"" then
                    imm.Substring(1, imm.Length - 2) |> Str |> PushImmediate |> IType |> Some
                else if imm.StartsWith "'" then
                    imm.[1] |> Character |> PushImmediate |> IType |> Some
                else if imm.Contains "." then
                    Convert.ToDouble imm |> Dbl |> PushImmediate |> IType |> Some
                else if isNumeric imm then
                    Convert.ToInt32 imm |> Integer |> PushImmediate |> IType |> Some
                else
                    match imm.ToLower () with
                        | "true" -> true |> Bool |> PushImmediate |> IType |> Some
                        | "false" -> false |> Bool |> PushImmediate |> IType |> Some
                        | _ -> PushImmediate Unit |> IType |> Some
            else None

        let itype =
            let _match = Regex.Match(line, @"\b(pop|push|poll)\s+([0-9]+)")
            if _match.Success then
                match _match.Groups.[1].Value with
                    | "pop" -> Convert.ToInt32 _match.Groups.[2].Value |> Pop |> IType |> Some
                    | "push" -> Convert.ToInt32 _match.Groups.[2].Value |> Push |> IType |> Some
                    | "poll" -> Convert.ToInt32 _match.Groups.[2].Value |> Poll |> IType |> Some
            else None

        let litype =
            let _match = Regex.Match(line, @"\b(alloc|label)\s+([a-zA-Z]+[0-9]*[a-zA-Z]*)\s+([0-9]+)")
            if _match.Success then
                let label = _match.Groups.[2].Value
                let imm = _match.Groups.[3].Value |> Convert.ToInt32
                match _match.Groups.[1].Value with
                    | "alloc" -> Allocate (label, imm) |> LIType |> Some
                    | "label" -> CreateLabel (label, imm) |> LIType |> Some
            else None

        match call with
            | Some instr -> instr
            | None ->
                match ltype with
                    | Some instr -> instr
                    | None ->
                        match valtype with
                            | Some instr -> instr
                            | None ->
                                match itype with
                                    | Some instr -> instr
                                    | None ->
                                        match litype with
                                            | Some instr -> instr
                                            | None ->
                                                sprintf "Invalid instruction %s" line
                                                |> ArgumentException |> raise