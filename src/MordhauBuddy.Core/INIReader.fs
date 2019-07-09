namespace MordhauBuddy.Core

open FParsec
open System

module INIReader =
    type INIKey = string

    type INIValue =
        | INIString of string option
        | INIFieldText of string * INIValue
        | INITuple of INIValue list
        | INIKV of string * INIValue
        | INISection of string * INIValue list

    type INIData = Map<string, Map<INIKey, INIValue>>

    module internal PrimitiveParsers =
        /// Parses text surrounded by zero or more white spaces but stopping at newline
        let ws p = spaces >>. p .>> (skipMany (pchar ' ' <|> pchar '\t'))

        let wstr t = ws (pstring t)
        let listBetweenStrings sOpen sClose pElement =
            between (pstring sOpen) (pstring sClose)
                (spaces >>. sepBy (pElement .>> spaces) ((wstr "," <|> wstr ";") .>> spaces))
        let identifier =
            many1Satisfy2 (fun ch -> Char.IsLetter(ch) || ch = '/')
                (fun ch -> Char.IsLetterOrDigit(ch) || ch = '.' || ch = '/' || ch = '_')
        let anyText =
            many1Satisfy
                (fun ch ->
                (not <| Char.IsWhiteSpace(ch))
                && not (ch = ')' || ch = '(' || ch = ']' || ch = '[' || ch = ',' || ch = ';' || ch = '=' || ch = '\n'))
        let hasKey = previousCharSatisfiesNot (fun ch -> ch = '=') >>. anyText .>>? wstr "="
        let hasFieldValue =
            previousCharSatisfies (fun ch -> ch = '=') >>? anyText .>>? nextCharSatisfies (fun ch -> ch = '(')
        let parseQuoted = pchar '"' >>. manySatisfy (fun c -> c <> '"') .>> pchar '"'
        let parseQuotedInc =
            pchar '"' .>>. manySatisfy (fun c -> c <> '"') .>>. pchar '"'
            |>> (fun ((c, s), c2) -> string c + s + string c2)

        let extractFail p str =
            match run p str with
            | Success(result, _, _) -> result
            | Failure(msg, _, _) -> failwith msg

        let extractOption p str =
            match run p str with
            | Success(result, _, _) -> Some result
            | Failure(msg, _, _) -> None

        let iValue, iValueRef = createParserForwardedToRef()
        let comment = pstring "#" >>. skipRestOfLine true
        let iniEmpty =
            pchar '\n' |>> ignore <|> previousCharSatisfies (fun ch -> ch = '=' || ch = ',' || ch = ')')
            |>> ((fun _ -> None) >> INIString)
        let iniString = parseQuoted <|> anyText |>> (Some >> INIString) .>> spaces
        let iniFieldText =
            hasFieldValue
            .>>. (listBetweenStrings "(" ")" parseQuotedInc |>> (List.map (Some >> INIString)) |>> INITuple)
            |>> INIFieldText
        let iniKV = hasKey .>>. iValue |>> INIKV
        let iniTuple = listBetweenStrings "(" ")" iValue |>> INITuple
        let iniSection =
            between (spaces >>. pstring "[") (pstring "]" .>> spaces) identifier
            .>>. many (skipMany comment >>. iValue .>> spaces) |>> INISection

        do iValueRef := choice [ iniTuple; iniKV; iniFieldText; iniString; iniEmpty ]

        let ini = many (spaces >>. skipMany comment >>. iniSection .>> spaces)

    module INIParser =
        open PrimitiveParsers

        /// Reads an INI AST from a string throwing an exception if it fails to parse.
        let read s = extractFail ini s

        /// Reads an INI AST from a string returning None if it fails to parse.
        let read2opt s = extractOption ini s

        let read2res text = run ini text

        /// Read an INI AST from a file, returning None if it fails to parse.
        let readFile =
            fun fname ->
                let text = System.IO.File.ReadAllText(fname)
                read2opt text

        let readFile2res fname =
            let text = System.IO.File.ReadAllText(fname)
            read2res text

    module INIExtr =
        let (>>=) ma fn = Option.bind fn ma

        type MaybeBuilder() =

            member this.Bind(ma, f) =
                match ma with
                | Some(a) -> f a
                | _ -> None

            member this.Delay(f) = f()
            member this.Return(x) = Some x

        let maybe = MaybeBuilder()

        let private parseInt (s : string) : int option =
            try
                Some(System.Int32.Parse s)
            with :? System.FormatException -> None

        let private parseBool (s : string) : bool option =
            try
                Some(System.Boolean.Parse s)
            with :? System.FormatException -> None

        let private parseDouble (s : string) : float option =
            try
                Some(System.Double.Parse s)
            with :? System.FormatException -> None

        let getINIString : INIValue -> string option =
            function
            | INIString x -> x
            | _ -> None

        let getINITuple : INIValue -> INIValue list option =
            function
            | INITuple xs -> Some xs
            | _ -> None

        let isINIString : INIValue -> bool =
            function
            | INIString _ -> true
            | _ -> false

        let isINITuple : INIValue -> bool =
            function
            | INITuple _ -> true
            | _ -> false

        let sequence mlist =
            let (>>=) = fun ma f -> Option.bind f ma
            let unit x = Option.Some x
            let mcons p q = p >>= fun x -> q >>= fun y -> unit (x :: y)
            List.foldBack mcons mlist (unit [])

        let applySequence fn xs = sequence <| List.map fn xs

        let fieldKV : string -> string -> INIData -> INIValue option =
            fun section key ast ->
                ast
                |> Map.tryFind section
                |> Option.bind (Map.tryFind key)

        let fieldString : string -> string -> INIData -> string option =
            fun section key -> fieldKV section key >> Option.bind getINIString
        let fieldInt section key ast = fieldString section key ast >>= parseInt
        let fieldDouble section key ast = fieldString section key ast >>= parseDouble
        let fieldBool section key ast = fieldString section key ast >>= parseBool

        /// Extracts a list of strings from an INI ast.
        ///
        /// ##Parameters
        ///
        /// - `section` - Section to be extracted from the AST.
        /// - `key`     - key within the section to be extracted.
        ///
        let fieldTupleOfString (section : string) (key : string) ast =
            ast
            |> fieldKV section key
            >>= getINITuple
            >>= applySequence getINIString
