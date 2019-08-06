namespace MordhauBuddy.Core

open FParsec
open FSharp.Data
open System
open System.ComponentModel
open System.Globalization
open System.IO

module rec INIReader =
    [<RequireQualifiedAccess>]
    [<StructuredFormatDisplay("{_Print}")>]
    type INIValue =
        | String of string option
        | FieldText of string * INIValue
        | Tuple of INIValue list
        | KeyValue of string * INIValue
        | Section of string * INIValue list
        | File of INIValue list

        /// [omit]
        [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
        [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden = true,
                                   IsError = false)>]
        member x._Print =
            match x.ToString() with
            | str when str.Length > 512 -> str.Substring(0, 509) + "..."
            | str -> str

        /// Serializes INIValue to TextWriter
        member this.WriteTo(w : TextWriter) =
            let rec serialize =
                function
                | String(Some(s)) -> w.Write(s)
                | String(None) -> ()
                | FieldText(s, iniValue) ->
                    w.Write(s)
                    serialize iniValue
                | Tuple(iList) ->
                    let len = iList.Length - 1
                    w.Write("(")
                    iList
                    |> List.indexed
                    |> List.iter (fun (ind, i) ->
                           serialize i
                           if ind < len then w.Write(","))
                    w.Write(")")
                | KeyValue(s, iniValue) ->
                    w.Write(s + "=")
                    serialize iniValue
                | Section(s, iList) ->
                    w.Write("[" + s + "]")
                    w.WriteLine()
                    iList
                    |> List.iter (fun i ->
                           serialize i
                           w.WriteLine())
                    w.WriteLine()
                | File(iList) -> iList |> List.iter serialize
            serialize this

        override this.ToString() : string =
            let w = new StringWriter(CultureInfo.InvariantCulture)
            this.WriteTo(w)
            w.GetStringBuilder().ToString()

    type private INIParser(iniText : string) =

        /// Parses text surrounded by zero or more white spaces but stopping at newline
        let ws p = spaces >>. p .>> (skipMany (pchar ' ' <|> pchar '\t'))

        /// ws helper
        let wstr t = ws (pstring t)

        /// Returns parser that is between two characters
        let listBetweenStrings sOpen sClose pElement =
            between (pstring sOpen) (pstring sClose)
                (spaces >>. sepBy (pElement .>> spaces) ((wstr "," <|> wstr ";") .>> spaces))

        /// Determines if it is a section header
        let identifier =
            many1Satisfy2 (fun ch -> Char.IsLetter(ch) || ch = '/')
                (fun ch -> Char.IsLetterOrDigit(ch) || ch = '.' || ch = '/' || ch = '_')

        /// Determines if the selection is text
        let anyText =
            many1Satisfy
                (fun ch ->
                (not <| Char.IsWhiteSpace(ch))
                && not (ch = ')' || ch = '(' || ch = ']' || ch = '[' || ch = ',' || ch = ';' || ch = '=' || ch = '\n'))

        /// Determines if it is a kv pair
        let hasKey = previousCharSatisfiesNot (fun ch -> ch = '=') >>. anyText .>>? wstr "="

        /// Determines if it is a field value
        let hasFieldValue =
            previousCharSatisfies (fun ch -> ch = '=') >>? anyText .>>? nextCharSatisfies (fun ch -> ch = '(')

        /// Parse quoted string returning without quotes
        let parseQuoted = pchar '"' >>. manySatisfy (fun c -> c <> '"') .>> pchar '"'

        /// Parse quoted string returning string with quotes
        let parseQuotedInc =
            pchar '"' .>>. manySatisfy (fun c -> c <> '"') .>>. pchar '"'
            |>> (fun ((c, s), c2) -> string c + s + string c2)

        /// Create parser and reference cell
        let iValue, iValueRef = createParserForwardedToRef()

        /// Parse comment
        let comment = pstring "#" >>. skipRestOfLine true

        /// Determine if the line is empty
        let iniEmpty =
            pchar '\n' |>> ignore <|> previousCharSatisfies (fun ch -> ch = '=' || ch = ',' || ch = ')')
            |>> ((fun _ -> None) >> INIValue.String)

        /// Parse a string
        let iniString = parseQuoted <|> anyText |>> (Some >> INIValue.String) .>> spaces

        /// Parse a field text value
        ///
        /// e.g. `SomeField("look at my text")`
        let iniFieldText =
            hasFieldValue
            .>>. (listBetweenStrings "(" ")" parseQuotedInc |>> (List.map (Some >> INIValue.String)) |>> INIValue.Tuple)
            |>> INIValue.FieldText

        /// Parse a key value pair
        let iniKV = hasKey .>>. iValue |>> INIValue.KeyValue

        /// Parse a tuple
        let iniTuple = listBetweenStrings "(" ")" iValue |>> INIValue.Tuple

        /// Parse a section
        let iniSection =
            between (spaces >>. pstring "[") (pstring "]" .>> spaces) identifier
            .>>. many (skipMany comment >>. iValue .>> spaces) |>> INIValue.Section

        do iValueRef := choice [ iniTuple; iniKV; iniFieldText; iniString; iniEmpty ]

        /// Parses all sections in the file
        let ini = many (spaces >>. skipMany comment >>. iniSection .>> spaces) |>> INIValue.File

        /// Parses text and fails on error
        member this.Parse() =
            match run ini iniText with
            | Success(result, _, _) ->
                match result with
                | INIValue.File([]) -> failwith "No sections found in file"
                | _ -> result
            | Failure(msg, _, _) -> failwith msg

        /// Parses text and returns an option
        member this.TryParse() =
            match run ini iniText with
            | Success(result, _, _) ->
                match result with
                | INIValue.File([]) -> None
                | _ -> result |> Some
            | Failure(msg, _, _) -> None

    type INIValue with

        /// Parses the specified INI string
        static member Parse(text : string) = INIParser(text).Parse()

        /// Attempts to parse the specified INI string
        static member TryParse(text : string) = INIParser(text).TryParse()

        /// Loads INI from the specified stream
        static member Load(stream : Stream) =
            use reader = new StreamReader(stream)
            let text = reader.ReadToEnd()
            INIParser(text).Parse()

    type INIConversions =

        static member AsString =
            function
            | INIValue.String(s) -> s
            | _ -> None

        static member AsInteger(cultureInfo : IFormatProvider) =
            function
            | INIValue.String(s) -> TextConversions.AsInteger cultureInfo <| defaultArg s ""
            | _ -> None

        static member AsInteger64(cultureInfo : IFormatProvider) =
            function
            | INIValue.String(s) -> TextConversions.AsInteger64 cultureInfo <| defaultArg s ""
            | _ -> None

        static member AsDecimal(cultureInfo : IFormatProvider) =
            function
            | INIValue.String(s) -> TextConversions.AsDecimal cultureInfo <| defaultArg s ""
            | _ -> None

        static member AsFloat (missingValues : string []) (useNoneForMissingValues : bool)
                      (cultureInfo : IFormatProvider) =
            function
            | INIValue.String(s) ->
                TextConversions.AsFloat missingValues useNoneForMissingValues cultureInfo <| defaultArg s ""
            | _ -> None

        static member AsBoolean =
            function
            | INIValue.String(s) when (defaultArg s "") = "1" -> Some(true)
            | INIValue.String(s) when (defaultArg s "") = "0" -> Some(false)
            | INIValue.String(s) -> TextConversions.AsBoolean <| defaultArg s ""
            | _ -> None

    module INIExtensions =
        open System.Runtime.CompilerServices
        open System.Runtime.InteropServices

        [<Extension>]
        /// Extension methods with operations on INI values
        type INIExtensions =

            /// Get a sequence of key-value pairs representing the properties of an object
            [<Extension>]
            static member Properties(x : INIValue) =
                match x with
                | INIValue.File(sList) -> ("File", sList)
                | INIValue.Section(s, vList) -> (s, vList)
                | INIValue.KeyValue(s, v) ->
                    match v with
                    | INIValue.Tuple(tList) -> (s, tList)
                    | _ -> (s, [ v ])
                | INIValue.FieldText(s, v) ->
                    match v with
                    | INIValue.Tuple(tList) -> (s, tList)
                    | _ -> (s, [ v ])
                | _ -> ("", [])

            /// Get property of an INI object. Fails if the value is not an object
            /// or if the property is not present
            [<Extension>]
            static member GetProperty(x, propertyName) =
                match INIExtensions.Properties x with
                | (s, pList) when pList.Length > 0 && s = propertyName -> pList
                | (_, pList) when pList.Length > 0 ->
                    failwithf "Didn't find property '%s' in %s" propertyName <| x.ToString()
                | _ -> failwithf "Not an object: %s" <| x.ToString()

            /// Try to get a property of a INI value.
            /// Returns None if the value is not an object or if the property is not present.
            [<Extension>]
            static member TryGetProperty(x, propertyName) =
                match INIExtensions.Properties x with
                | (s, pList) when pList.Length > 0 && s = propertyName -> Some(pList)
                | _ -> None

            /// Assuming the value is an object, get value with the specified name
            [<Extension>]
            static member inline Item(x, propertyName) = INIExtensions.GetProperty(x, propertyName)

            /// Get all the elements of an INI value.
            /// Returns an empty list if the value is not an INI.
            [<Extension>]
            static member AsList(x : INIValue) =
                match x with
                | INIValue.File(sList) -> sList
                | INIValue.Section(_, vList) -> vList
                | INIValue.KeyValue(_, v) ->
                    match v with
                    | INIValue.Tuple(tList) -> tList
                    | _ -> [ v ]
                | INIValue.Tuple(tList) -> tList
                | INIValue.FieldText(_, v) -> [ v ]
                | _ -> []

            /// Get all the elements of a INI value (assuming that the value is an array)
            [<Extension>]
            static member inline GetEnumerator(x) =
                INIExtensions.AsList(x)
                |> Array.ofList
                |> (fun a -> a.GetEnumerator())

            /// Try to get the value at the specified index, if the value is a INI array.
            [<Extension>]
            static member inline Item(x, index) = INIExtensions.AsList(x).[index]

            /// Get the string value of an element (assuming that the value is a scalar)
            /// Returns the empty string for INIValue.Null
            [<Extension>]
            static member AsString(x) =
                match INIConversions.AsString x with
                | Some s -> s
                | _ -> failwithf "Not a string: %s" <| x.ToString()

            /// Get a number as an integer (assuming that the value fits in integer)
            [<Extension>]
            static member AsInteger(x, [<Optional>] ?cultureInfo) =
                let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                match INIConversions.AsInteger cultureInfo x with
                | Some i -> i
                | _ -> failwithf "Not an int: %s" <| x.ToString()

            /// Get a number as a 64-bit integer (assuming that the value fits in 64-bit integer)
            [<Extension>]
            static member AsInteger64(x, [<Optional>] ?cultureInfo) =
                let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                match INIConversions.AsInteger64 cultureInfo x with
                | Some i -> i
                | _ -> failwithf "Not an int64: %s" <| x.ToString()

            /// Get a number as a decimal (assuming that the value fits in decimal)
            [<Extension>]
            static member AsDecimal(x, [<Optional>] ?cultureInfo) =
                let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                match INIConversions.AsDecimal cultureInfo x with
                | Some d -> d
                | _ -> failwithf "Not a decimal: %s" <| x.ToString()

            /// Get a number as a float (assuming that the value is convertible to a float)
            [<Extension>]
            static member AsFloat(x, [<Optional>] ?cultureInfo, [<Optional>] ?missingValues) =
                let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                let missingValues = defaultArg missingValues TextConversions.DefaultMissingValues
                match INIConversions.AsFloat missingValues false cultureInfo x with
                | Some f -> f
                | _ -> failwithf "Not a float: %s" <| x.ToString()

            /// Get the boolean value of an element (assuming that the value is a boolean)
            [<Extension>]
            static member AsBoolean(x) =
                match INIConversions.AsBoolean x with
                | Some b -> b
                | _ -> failwithf "Not a boolean: %s" <| x.ToString()

            /// Get inner text of an element
            [<Extension>]
            static member InnerText(x) =
                match INIConversions.AsString x with
                | Some str -> str
                | None ->
                    INIExtensions.AsList(x)
                    |> List.map (fun e -> INIExtensions.InnerText(e))
                    |> String.Concat

            /// Map INIValue based on matching conditions
            [<Extension>]
            static member Map(matchConditions : string list, ast : INIValue, newValue : INIValue) =
                let rec mapAst (s : string list) (iVal : INIValue) : INIValue =
                    let testVal (x : INIValue) : bool =
                        match x with
                        | INIValue.Section(n, _) -> n <> (s.Head)
                        | INIValue.FieldText(n, _) -> n <> (s.Head)
                        | INIValue.KeyValue(n, _) -> n <> (s.Head)
                        | INIValue.String(n) -> defaultArg n "" <> s.Head
                        | _ -> false

                    let getResults (iList : INIValue list) (sendTail : bool) =
                        let matchers =
                            match sendTail with
                            | true -> s.Tail
                            | false -> s
                        if iList.IsEmpty && s.Head = "()" then [ mapAst (s.Tail) (INIValue.String(None)) ]
                        else
                            iList
                            |> List.map (fun iVal ->
                                   match testVal iVal with
                                   | false -> mapAst matchers iVal
                                   | true -> iVal)

                    match iVal with
                    | _ when s.IsEmpty -> newValue
                    | INIValue.File e -> getResults e true |> INIValue.File
                    | INIValue.Section(secName, secL) ->
                        getResults secL false |> (fun res -> INIValue.Section(secName, res))
                    | INIValue.KeyValue(kName, kL) -> mapAst (s.Tail) kL |> (fun res -> INIValue.KeyValue(kName, res))
                    | INIValue.Tuple(tL) -> getResults tL false |> INIValue.Tuple
                    | INIValue.FieldText(fName, fTup) ->
                        match fName = s.Head with
                        | true -> mapAst (s.Tail) fTup
                        | false -> fTup
                        |> (fun res -> INIValue.FieldText(fName, res))
                    | INIValue.String(_) as kvStr ->
                        match testVal kvStr with
                        | true -> kvStr
                        | false -> mapAst (s.Tail) kvStr
                mapAst matchConditions ast

        /// Get a property of a INI object
        let (?) (iValue : INIValue) propertyName = iValue.GetProperty(propertyName) |> List.head

        type INIValue with
            member this.Properties =
                let props x =
                    match x with
                    | INIValue.File(sList) -> ("", sList)
                    | INIValue.Section(s, vList) -> (s, vList)
                    | INIValue.KeyValue(s, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> (s, tList)
                        | _ -> (s, [ v ])
                    | INIValue.FieldText(s, v) -> (s, [ v ])
                    | _ -> ("", [])
                props this
                |> (fun (_, v) -> v)
                |> List.map props

        /// Extension methods that can be used to work with INIValue in more convenient way.
        /// This module also provides the dynamic operator.
        module Options =
            type INIValue with

                /// Get a sequence of key-value pairs representing the properties of an object
                member this.Properties =
                    match this with
                    | INIValue.File(sList) -> ("File", sList)
                    | INIValue.Section(s, vList) -> (s, vList)
                    | INIValue.KeyValue(s, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> (s, tList)
                        | _ -> (s, [ v ])
                    | INIValue.FieldText(s, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> (s, tList)
                        | _ -> (s, [ v ])
                    | _ -> ("", [])

                /// Get property of an INI object. Fails if the value is not an object
                /// or if the property is not present
                member this.GetProperty(propertyName) =
                    match INIExtensions.Properties this with
                    | (s, pList) when pList.Length > 0 && s = propertyName -> pList
                    | (_, pList) when pList.Length > 0 ->
                        failwithf "Didn't find property '%s' in %s" propertyName <| this.ToString()
                    | _ -> failwithf "Not an object: %s" <| this.ToString()

                /// Try to get a property of a INI value.
                /// Returns None if the value is not an object or if the property is not present.
                member this.TryGetProperty(propertyName) =
                    match INIExtensions.Properties this with
                    | (s, pList) when pList.Length > 0 && s = propertyName -> Some(pList)
                    | _ -> None

                /// Assuming the value is an object, get value with the specified name
                member inline this.Item(propertyName) = INIExtensions.GetProperty(this, propertyName)

                /// Get all the elements of an INI value.
                /// Returns an empty list if the value is not an INI.
                member this.AsList() =
                    match this with
                    | INIValue.File(sList) -> sList
                    | INIValue.Section(_, vList) -> vList
                    | INIValue.KeyValue(_, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> tList
                        | _ -> [ v ]
                    | INIValue.Tuple(tList) -> tList
                    | INIValue.FieldText(_, v) -> [ v ]
                    | _ -> []

                /// Get all the elements of a INI value (assuming that the value is an array)
                member inline this.GetEnumerator() =
                    INIExtensions.AsList(this)
                    |> Array.ofList
                    |> (fun a -> a.GetEnumerator())

                /// Try to get the value at the specified index, if the value is a INI array.
                member inline this.Item(index) = INIExtensions.AsList(this).[index]

                /// Get the string value of an element (assuming that the value is a scalar)
                /// Returns the empty string for INIValue.Null
                member this.AsString() =
                    match INIConversions.AsString this with
                    | Some s -> s
                    | _ -> failwithf "Not a string: %s" <| this.ToString()

                /// Get a number as an integer (assuming that the value fits in integer)
                member this.AsInteger([<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    match INIConversions.AsInteger cultureInfo this with
                    | Some i -> i
                    | _ -> failwithf "Not an int: %s" <| this.ToString()

                /// Get a number as a 64-bit integer (assuming that the value fits in 64-bit integer)
                member this.AsInteger64([<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    match INIConversions.AsInteger64 cultureInfo this with
                    | Some i -> i
                    | _ -> failwithf "Not an int64: %s" <| this.ToString()

                /// Get a number as a decimal (assuming that the value fits in decimal)
                member this.AsDecimal([<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    match INIConversions.AsDecimal cultureInfo this with
                    | Some d -> d
                    | _ -> failwithf "Not a decimal: %s" <| this.ToString()

                /// Get a number as a float (assuming that the value is convertible to a float)
                member this.AsFloat([<Optional>] ?cultureInfo, [<Optional>] ?missingValues) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    let missingValues = defaultArg missingValues TextConversions.DefaultMissingValues
                    match INIConversions.AsFloat missingValues false cultureInfo this with
                    | Some f -> f
                    | _ -> failwithf "Not a float: %s" <| this.ToString()

                /// Get the boolean value of an element (assuming that the value is a boolean)
                member this.AsBoolean() =
                    match INIConversions.AsBoolean this with
                    | Some b -> b
                    | _ -> failwithf "Not a boolean: %s" <| this.ToString()

                /// Get inner text of an element
                member this.InnerText() =
                    match INIConversions.AsString this with
                    | Some str -> str
                    | None ->
                        INIExtensions.AsList(this)
                        |> List.map (fun e -> INIExtensions.InnerText(e))
                        |> String.Concat

                /// Map INIValue based on matching conditions
                member this.Map(matchConditions : string list, newValue : INIValue) =
                    INIExtensions.Map(matchConditions, this, newValue)

            [<Extension>]
            [<AbstractClass>]
            type INIValueOptionExtensions() =

                /// Get a sequence of key-value pairs representing the properties of an object
                [<Extension>]
                static member Properties(x : INIValue) =
                    match x with
                    | INIValue.File(sList) -> ("File", sList)
                    | INIValue.Section(s, vList) -> (s, vList)
                    | INIValue.KeyValue(s, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> (s, tList)
                        | _ -> (s, [ v ])
                    | INIValue.FieldText(s, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> (s, tList)
                        | _ -> (s, [ v ])
                    | _ -> ("", [])

                /// Try to get a property of a INI value.
                /// Returns None if the value is not an object or if the property is not present.
                [<Extension>]
                static member TryGetProperty(x, propertyName) =
                    match INIExtensions.Properties x with
                    | (s, pList) when pList.Length > 0 && s = propertyName -> Some(pList)
                    | _ -> None

                /// Try to get a property of a INI value.
                /// Returns None if the value is not a INI object or if the property is not present.
                [<Extension>]
                static member inline Item(x, propertyName) = INIValueOptionExtensions.TryGetProperty(x, propertyName)

                /// Get all the elements of a INI value.
                /// Returns an empty array if the value is not a INI array.
                [<Extension>]
                static member AsList(x : INIValue) =
                    match x with
                    | INIValue.File(sList) -> sList
                    | INIValue.Section(_, vList) -> vList
                    | INIValue.KeyValue(_, v) ->
                        match v with
                        | INIValue.Tuple(tList) -> tList
                        | _ -> [ v ]
                    | INIValue.Tuple(tList) -> tList
                    | INIValue.FieldText(_, v) -> [ v ]
                    | _ -> []

                /// Get all the elements of a INI value (assuming that the value is an array)
                [<Extension>]
                static member inline GetEnumerator(x) =
                    INIExtensions.AsList(x)
                    |> Array.ofList
                    |> (fun a -> a.GetEnumerator())

                /// Try to get the value at the specified index, if the value is a INI array.
                [<Extension>]
                static member inline Item(x, index) = INIValueOptionExtensions.AsList(x).[index]

                /// Get the string value of an element (assuming that the value is a scalar)
                [<Extension>]
                static member AsString(x) = x |> Option.bind INIConversions.AsString

                /// Get a number as an integer (assuming that the value fits in integer)
                [<Extension>]
                static member AsInteger(x, [<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    x |> Option.bind (INIConversions.AsInteger cultureInfo)

                /// Get a number as a 64-bit integer (assuming that the value fits in 64-bit integer)
                [<Extension>]
                static member AsInteger64(x, [<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    x |> Option.bind (INIConversions.AsInteger64 cultureInfo)

                /// Get a number as a decimal (assuming that the value fits in decimal)
                [<Extension>]
                static member AsDecimal(x, [<Optional>] ?cultureInfo) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    x |> Option.bind (INIConversions.AsDecimal cultureInfo)

                /// Get a number as a float (assuming that the value is convertible to a float)
                [<Extension>]
                static member AsFloat(x, [<Optional>] ?cultureInfo, [<Optional>] ?missingValues) =
                    let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
                    let missingValues = defaultArg missingValues TextConversions.DefaultMissingValues
                    x |> Option.bind (INIConversions.AsFloat missingValues (*useNoneForMissingValues*) true cultureInfo)

                /// Get the boolean value of an element (assuming that the value is a boolean)
                [<Extension>]
                static member AsBoolean(x) = x |> Option.bind INIConversions.AsBoolean

                /// Get inner text of an element
                [<Extension>]
                static member InnerText(x) =
                    x
                    |> Option.bind (fun x ->
                           match INIValueOptionExtensions.AsString(x) with
                           | Some str -> Some(str)
                           | None ->
                               INIValueOptionExtensions.AsList(x.Value)
                               |> List.map (fun e -> e.InnerText())
                               |> String.Concat
                               |> Some)

                /// Map INIValue based on matching conditions
                [<Extension>]
                static member Map(matchConditions : string list, ast : INIValue option, newValue : INIValue) =
                    ast |> Option.bind (fun res -> INIExtensions.Map(matchConditions, res, newValue) |> Some)

            /// [omit]
            type INIValueOverloads = INIValueOverloads
                with
                    static member inline ($) (x : INIValue, INIValueOverloads) =
                        fun propertyName -> x.TryGetProperty propertyName |> Option.map (List.head)
                    static member inline ($) (x : INIValue option, INIValueOverloads) =
                        fun propertyName -> x |> Option.bind (fun x -> x.TryGetProperty propertyName |> Option.map (List.head))

            /// Get property of a INI value (assuming that the value is an object)
            let inline (?) x (propertyName : string) = (x $ INIValueOverloads) propertyName
