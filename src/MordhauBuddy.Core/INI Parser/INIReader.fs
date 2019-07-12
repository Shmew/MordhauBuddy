namespace MordhauBuddy.Core.INIReader

open FParsec
open System
open System.ComponentModel
open System.Globalization
open System.IO
open FSharp.Data

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
    [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden=true, IsError=false)>]
    member x._Print =
      match x.ToString() with
      | str when str.Length > 512 -> str.Substring(0, 509) + "..."
      | str -> str

    /// Serializes INIValue to TextWriter
    member this.WriteTo (w: TextWriter) =
        let rec serialize = function
        | String(Some(s)) -> w.Write(s)
        | String(None) -> ()
        | FieldText(s,iniValue) ->
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
        | File(iList) ->
            iList |> List.iter serialize

        serialize this

    override this.ToString () : string =
        let w = new StringWriter(CultureInfo.InvariantCulture)
        this.WriteTo(w)
        w.GetStringBuilder().ToString()

type private INIParser(iniText: string) =
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
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

    /// Parses text and returns an option
    member this.TryParse() =
        match run ini iniText with
        | Success(result, _, _) -> Some result
        | Failure(msg, _, _) -> None

type INIValue with

    /// Parses the specified INI string
    static member Parse(text: string) =
        INIParser(text).Parse()

    /// Attempts to parse the specified INI string
    static member TryParse(text: string) =
        INIParser(text).TryParse()
        
    /// Loads INI from the specified stream
    static member Load(stream: Stream) =
        use reader = new StreamReader(stream)
        let text = reader.ReadToEnd()
        INIParser(text).Parse()

type INIConversions =

    static member AsString = function
        | INIValue.String(s) -> s
        | _ -> None

    static member AsInteger (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsInteger cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsInteger64 (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsInteger64 cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsDecimal (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsDecimal cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsFloat (missingValues: string []) (useNoneForMissingValues: bool) (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsFloat missingValues useNoneForMissingValues cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsBoolean = function
        | INIValue.String(s) when (defaultArg s "") = "1" -> Some(true)
        | INIValue.String(s) when (defaultArg s "") = "0" -> Some(false)
        | INIValue.String(s) -> TextConversions.AsBoolean <| defaultArg s ""
        | _ -> None

    static member AsDateTimeOffset (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsDateTimeOffset cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsDateTime (cultureInfo: IFormatProvider) = function
        | INIValue.String(s) -> TextConversions.AsDateTime cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsTimeSpan (cultureInfo: CultureInfo) = function
        | INIValue.String(s) -> TextConversions.AsTimeSpan cultureInfo <| defaultArg s ""
        | _ -> None

    static member AsGuid = function
        | INIValue.String(s) -> TextConversions.AsGuid <| defaultArg s ""
        | _ -> None
