namespace MordhauBuddy.Core.INIReader

open System
open System.Globalization
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FSharp.Data

[<Extension>]
/// Extension methods with operations on INI values
type INIExtensions =

    /// Get a sequence of key-value pairs representing the properties of an object
    [<Extension>]
    static member Properties(x:INIValue) =
        let props x =
            match x with
            | INIValue.File(sList) -> ("",sList)
            | INIValue.Section(s,vList) -> (s,vList)
            | INIValue.KeyValue(s,v) -> (s,[v])
            | INIValue.FieldText(s,v) -> (s,[v])
            | _ -> ("",[])

        props x |> (fun (_,v) -> v) |> List.map props

    /// Get property of an INI object. Fails if the value is not an object
    /// or if the property is not present
    [<Extension>]
    static member GetProperty(x, propertyName) =
        match INIExtensions.Properties x with
        | pList when pList.Length > 0 ->
            match List.tryFind (fst >> (=) propertyName) pList with 
            | Some (_, value) -> value
            | None -> failwithf "Didn't find property '%s' in %s" propertyName <| x.ToString()
        | _ -> failwithf "Not an object: %s" <| x.ToString()

    /// Try to get a property of a INI value.
    /// Returns None if the value is not an object or if the property is not present.
    [<Extension>]
    static member TryGetProperty(x, propertyName) = 
        match INIExtensions.Properties x with
        | pList when pList.Length > 0 ->
            List.tryFind (fst >> (=) propertyName) pList |> Option.map snd 
        | _ -> None

    /// Assuming the value is an object, get value with the specified name
    [<Extension>] 
    static member inline Item(x, propertyName) = INIExtensions.GetProperty(x, propertyName)

    /// Get all the elements of an INI value.
    /// Returns an empty list if the value is not an INI.
    [<Extension>]
    static member AsList(x:INIValue) = 
        match x with
        | INIValue.File(sList) -> sList
        | INIValue.Section(_,vList) -> vList
        | INIValue.KeyValue(_,v) -> [v]
        | INIValue.FieldText(_,v) -> [v]
        | _ -> []

    /// Get all the elements of a INI value (assuming that the value is an array)
    [<Extension>] 
    static member inline GetEnumerator(x) = INIExtensions.AsList(x) |> Array.ofList |> (fun a -> a.GetEnumerator())

    /// Try to get the value at the specified index, if the value is a INI array.
    [<Extension>] 
    static member inline Item(x, index) = INIExtensions.AsList(x).[index]

    /// Get the string value of an element (assuming that the value is a scalar)
    /// Returns the empty string for INIValue.Null
    [<Extension>] 
    static member AsString(x, [<Optional>] ?cultureInfo) =
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

    /// Get the datetime value of an element (assuming that the value is a string
    /// containing well-formed ISO date or MSFT INI date)
    [<Extension>]
    static member AsDateTime(x, [<Optional>] ?cultureInfo) = 
        let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
        match INIConversions.AsDateTime cultureInfo x with
        | Some d -> d
        | _ -> failwithf "Not a datetime: %s" <| x.ToString()

    /// Get the datetime offset value of an element (assuming that the value is a string
    /// containing well-formed ISO date time with offset or MSFT INI datetime with offset)
    [<Extension>]
    static member AsDateTimeOffset(x, [<Optional>] ?cultureInfo) = 
        let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
        match INIConversions.AsDateTimeOffset cultureInfo x with
        | Some d -> d
        | _ -> failwithf "Not a datetime offset: %s" <| x.ToString()

    /// Get the timespan value of an element (assuming that the value is a string
    /// containing well-formed time span)
    [<Extension>]
    static member AsTimeSpan(x, [<Optional>] ?cultureInfo) = 
        let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
        match INIConversions.AsTimeSpan cultureInfo x with
        | Some t -> t
        | _ -> failwithf "Not a time span: %s" <| x.ToString()

    /// Get the guid value of an element (assuming that the value is a guid)
    [<Extension>]
    static member AsGuid(x) =
        match INIConversions.AsGuid x with
        | Some g -> g
        | _ -> failwithf "Not a guid: %s" <| x.ToString()

    /// Get inner text of an element
    [<Extension>]
    static member InnerText(x) = 
        match INIConversions.AsString x with
        | Some str -> str
        | None -> INIExtensions.AsList(x) |> List.map (fun e -> INIExtensions.InnerText(e)) |> String.Concat

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
/// Provides the dynamic operator for getting a property of a INI object
module INIExtensions =

    /// Get a property of a INI object  
    let (?) (jsonObject:INIValue) propertyName = jsonObject.GetProperty(propertyName)

    type INIValue with
        member x.Properties =
            let props x =
                match x with
                | INIValue.File(sList) -> ("",sList)
                | INIValue.Section(s,vList) -> (s,vList)
                | INIValue.KeyValue(s,v) -> (s,[v])
                | INIValue.FieldText(s,v) -> (s,[v])
                | _ -> ("",[])

            props x |> (fun (_,v) -> v) |> List.map props

/// Extension methods that can be used to work with INIValue in more convenient way.
/// This module also provides the dynamic operator.
module Options = 
  
    type INIValue with
  
        /// Get a sequence of key-value pairs representing the properties of an object
        member this.Properties =
            let props x =
                match x with
                | INIValue.File(sList) -> ("",sList)
                | INIValue.Section(s,vList) -> (s,vList)
                | INIValue.KeyValue(s,v) -> (s,[v])
                | INIValue.FieldText(s,v) -> (s,[v])
                | _ -> ("",[])

            props this |> (fun (_,v) -> v) |> List.map props

        /// Get property of an INI object. Fails if the value is not an object
        /// or if the property is not present
        member this.GetProperty(propertyName) =
            match INIExtensions.Properties this with
            | pList when pList.Length > 0 ->
                match List.tryFind (fst >> (=) propertyName) pList with 
                | Some (_, value) -> value
                | None -> failwithf "Didn't find property '%s' in %s" propertyName <| this.ToString()
            | _ -> failwithf "Not an object: %s" <| this.ToString()

        /// Try to get a property of a INI value.
        /// Returns None if the value is not an object or if the property is not present.
        member this.TryGetProperty(propertyName) = 
            match INIExtensions.Properties this with
            | pList when pList.Length > 0 ->
                List.tryFind (fst >> (=) propertyName) pList |> Option.map snd 
            | _ -> None

        /// Assuming the value is an object, get value with the specified name
        member inline this.Item(propertyName) = INIExtensions.GetProperty(this, propertyName)

        /// Get all the elements of an INI value.
        /// Returns an empty list if the value is not an INI.
        member this.AsList() = 
            match this with
            | INIValue.File(sList) -> sList
            | INIValue.Section(_,vList) -> vList
            | INIValue.KeyValue(_,v) -> [v]
            | INIValue.FieldText(_,v) -> [v]
            | _ -> []

        /// Get all the elements of a INI value (assuming that the value is an array)
        member inline this.GetEnumerator() = INIExtensions.AsList(this) |> Array.ofList |> (fun a -> a.GetEnumerator())

        /// Try to get the value at the specified index, if the value is a INI array.
        member inline this.Item(index) = INIExtensions.AsList(this).[index]

        /// Get the string value of an element (assuming that the value is a scalar)
        /// Returns the empty string for INIValue.Null
        member this.AsString([<Optional>] ?cultureInfo) =
            let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
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

        /// Get the datetime value of an element (assuming that the value is a string
        /// containing well-formed ISO date or MSFT INI date)
        member this.AsDateTime([<Optional>] ?cultureInfo) = 
            let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
            match INIConversions.AsDateTime cultureInfo this with
            | Some d -> d
            | _ -> failwithf "Not a datetime: %s" <| this.ToString()

        /// Get the datetime offset value of an element (assuming that the value is a string
        /// containing well-formed ISO date time with offset or MSFT INI datetime with offset)
        member this.AsDateTimeOffset([<Optional>] ?cultureInfo) = 
            let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
            match INIConversions.AsDateTimeOffset cultureInfo this with
            | Some d -> d
            | _ -> failwithf "Not a datetime offset: %s" <| this.ToString()

        /// Get the timespan value of an element (assuming that the value is a string
        /// containing well-formed time span)
        member this.AsTimeSpan([<Optional>] ?cultureInfo) = 
            let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
            match INIConversions.AsTimeSpan cultureInfo this with
            | Some t -> t
            | _ -> failwithf "Not a time span: %s" <| this.ToString()

        /// Get the guid value of an element (assuming that the value is a guid)
        member this.AsGuid() =
            match INIConversions.AsGuid this with
            | Some g -> g
            | _ -> failwithf "Not a guid: %s" <| this.ToString()

        /// Get inner text of an element
        member this.InnerText() = 
            match INIConversions.AsString this with
            | Some str -> str
            | None -> INIExtensions.AsList(this) |> List.map (fun e -> INIExtensions.InnerText(e)) |> String.Concat
  
    [<Extension>] 
    [<AbstractClass>]
    type INIValueOptionExtensions() = 
  
        /// Get a sequence of key-value pairs representing the properties of an object
        [<Extension>] 
        static member Properties(x:INIValue) =
            let props x =
                match x with
                | INIValue.File(sList) -> ("",sList)
                | INIValue.Section(s,vList) -> (s,vList)
                | INIValue.KeyValue(s,v) -> (s,[v])
                | INIValue.FieldText(s,v) -> (s,[v])
                | _ -> ("",[])

            props x |> (fun (_,v) -> v) |> List.map props
  
        /// Try to get a property of a INI value.
        /// Returns None if the value is not an object or if the property is not present.
        [<Extension>] 
        static member TryGetProperty(x, propertyName) = 
            match INIExtensions.Properties x with
            | pList when pList.Length > 0 ->
                List.tryFind (fst >> (=) propertyName) pList |> Option.map snd 
            | _ -> None
  
        /// Try to get a property of a INI value.
        /// Returns None if the value is not a INI object or if the property is not present.
        [<Extension>] 
        static member inline Item(x, propertyName) = INIValueOptionExtensions.TryGetProperty(x, propertyName)
  
        /// Get all the elements of a INI value.
        /// Returns an empty array if the value is not a INI array.
        [<Extension>] 
        static member AsList(x:INIValue) = 
            match x with
            | INIValue.File(sList) -> sList
            | INIValue.Section(_,vList) -> vList
            | INIValue.KeyValue(_,v) -> [v]
            | INIValue.FieldText(_,v) -> [v]
            | _ -> []

        /// Get all the elements of a INI value (assuming that the value is an array)
        [<Extension>] 
        static member inline GetEnumerator(x) = INIExtensions.AsList(x) |> Array.ofList |> (fun a -> a.GetEnumerator())
  
        /// Try to get the value at the specified index, if the value is a INI array.
        [<Extension>] 
        static member inline Item(x, index) = INIValueOptionExtensions.AsList(x).[index]
  
        /// Get the string value of an element (assuming that the value is a scalar)
        [<Extension>] 
        static member AsString(x, [<Optional>] ?cultureInfo) =
            x |> Option.bind INIConversions.AsString
  
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
            x |> Option.bind (INIConversions.AsFloat missingValues (*useNoneForMissingValues*)true cultureInfo)
  
        /// Get the boolean value of an element (assuming that the value is a boolean)
        [<Extension>] 
        static member AsBoolean(x, [<Optional>] ?cultureInfo) =
            x |> Option.bind INIConversions.AsBoolean
  
        /// Get the datetime value of an element (assuming that the value is a string
        /// containing well-formed ISO date or MSFT INI date)
        [<Extension>] 
        static member AsDateTime(x, [<Optional>] ?cultureInfo) = 
            let cultureInfo = defaultArg cultureInfo CultureInfo.InvariantCulture
            x |> Option.bind (INIConversions.AsDateTime cultureInfo)

        /// Get the datetime offset value of an element (assuming that the value is a string
        /// containing well-formed ISO date time with offset)
        [<Extension>] 
        static member AsDateTimeOffset(x, [<Optional>] ?cultureInfo) = 
            let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
            x |> Option.bind (INIConversions.AsDateTimeOffset cultureInfo)

        /// Get the timespan value of an element (assuming that the value is a timespan)
        [<Extension>] 
        static member AsTimeSpan(x, [<Optional>] ?cultureInfo) =
            let cultureInfo = defaultArg cultureInfo  CultureInfo.InvariantCulture
            x |> Option.bind (INIConversions.AsTimeSpan cultureInfo) 
  
        /// Get the guid value of an element (assuming that the value is a guid)
        [<Extension>] 
        static member AsGuid(x) =
            x |> Option.bind INIConversions.AsGuid
  
        /// Get inner text of an element
        [<Extension>] 
        static member InnerText(x) =
            x |> Option.bind (fun x ->
            match INIValueOptionExtensions.AsString(x) with
            | Some str -> Some(str)
            | None -> INIValueOptionExtensions.AsList(x.Value) |> List.map (fun e -> e.InnerText()) |> String.Concat |> Some) 
  
    /// [omit]
    type INIValueOverloads = INIValueOverloads with
        static member inline ($) (x:INIValue                 , INIValueOverloads) = fun propertyName -> x.TryGetProperty propertyName
        static member inline ($) (x:INIValue option          , INIValueOverloads) = fun propertyName -> x |> Option.bind (fun x -> x.TryGetProperty propertyName)
  
    /// Get property of a INI value (assuming that the value is an object)
    let inline (?) x (propertyName:string) = (x $ INIValueOverloads) propertyName