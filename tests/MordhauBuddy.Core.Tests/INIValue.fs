namespace MordhauBuddy.Core.Tests

module INIValue =
    open Expecto
    open MordhauBuddy.Core
    open INIReader
    open INIReader.INIExtensions
    open System
    open System.Text.RegularExpressions

    [<AutoOpen>]
    module Utils =
        let fileOne iVal = INIValue.File([ iVal ])
        let fileList iList = INIValue.File(iList)
        let iString s = INIValue.String(Some(s))
        let iTupleOne iVal = INIValue.Tuple([ iVal ])
        let kvPair s1 s2 = INIValue.KeyValue(s1, iString s2)
        let kvPairNestedOne s nest = INIValue.KeyValue(s, iTupleOne nest)
        let kvPairNested s nest = INIValue.KeyValue(s, nest)
        let section s kvList = INIValue.Section(s, kvList)
        let sectionOne s iVal = INIValue.Section(s, [ iVal ])
        let fieldText s1 s2 = INIValue.FieldText(s1, iString ("\"" + s2 + "\"") |> iTupleOne)
        let serialI iVal = iVal.ToString()
        let lineEndings s = Regex.Replace(s, @"\r\n?|\n", "\r\n")

        let iStr s =
            sprintf "[TestSection]\nTestStr=%s\n" s
            |> INIValue.Parse

        let iStrSelectors = [ "TestSection"; "TestStr" ]

        let buildINIString (sList: string list) =
            sList
            |> List.reduce (fun acc elem -> acc + "\n" + elem)
            |> fun s -> s + "\n\n"
            |> lineEndings

    module OptionTests =
        open INIReader.INIExtensions.Options

        let map (oVal: INIValue) (selectors: string list) (nVal: INIValue) = oVal.Map(selectors, nVal)

    let parsing =
        [ testCase "Fails to parse empty document" <| fun () ->
              let result = INIValue.TryParse ""
              let expected = None
              Expect.equal result expected ""
          testCase "Fails to parse document without sections" <| fun () ->
              let result = INIValue.TryParse "Test=TestValue"
              let expected = None
              Expect.equal result expected ""
          testCase "Can parse document with single kv pair" <| fun () ->
              let result =
                  [ "[TestSection]"; "Test=TestValue" ]
                  |> buildINIString
                  |> INIValue.Parse

              let expected =
                  kvPair "Test" "TestValue"
                  |> sectionOne "TestSection"
                  |> fileOne

              Expect.equal result expected ""
          testCase "Can parse document with section and multiple kv pairs" <| fun () ->
              let result =
                  [ "[TestSection]"; "TestStr=Wowie"; "TestInt=1" ]
                  |> buildINIString
                  |> INIValue.Parse

              let expected =
                  [ kvPair "TestStr" "Wowie"
                    kvPair "TestInt" "1" ]
                  |> section "TestSection"
                  |> fileOne

              Expect.equal result expected ""
          testCase "Can parse document with multiple sections" <| fun () ->
              let result =
                  [ "[TestSection]"; "TestStr=Wowie"; "TestInt=1"; "[TestSection2]"; "TestStr=Wowza"; "TestInt2=2" ]
                  |> buildINIString
                  |> INIValue.Parse

              let expected =
                  let section1 =
                      [ kvPair "TestStr" "Wowie"
                        kvPair "TestInt" "1" ]
                      |> section "TestSection"

                  let section2 =
                      [ kvPair "TestStr" "Wowza"
                        kvPair "TestInt2" "2" ]
                      |> section "TestSection2"

                  INIValue.File([ section1; section2 ])

              Expect.equal result expected ""
          testCase "Can parse document with nested sections" <| fun () ->
              let result =
                  [ "[TestSection]"; "TestStr=(Name=TextField(\"tfValue\"))" ]
                  |> buildINIString
                  |> INIValue.Parse

              let expected =
                  fieldText "TextField" "tfValue"
                  |> kvPairNested "Name"
                  |> kvPairNestedOne "TestStr"
                  |> sectionOne "TestSection"
                  |> fileOne

              Expect.equal result expected ""
          testCase "Can parse document with empty kv pair" <| fun () ->
              let result =
                  [ "[TestSection]"; "TestStr=" ]
                  |> buildINIString
                  |> INIValue.Parse

              let expected =
                  INIValue.KeyValue("TestStr", INIValue.String(None))
                  |> sectionOne "TestSection"
                  |> fileOne

              Expect.equal result expected ""
          testCase "Can parse snippet" <| fun () ->
              let result =
                  [ "TestStr=(Name=TextField(\"tfValue\"))" ]
                  |> buildINIString
                  |> INIValue.ParseSnippet

              let expected =
                  fieldText "TextField" "tfValue"
                  |> kvPairNested "Name"
                  |> kvPairNestedOne "TestStr"

              Expect.equal result expected "" ]
        |> testList "INI Parsing"

    let serializing =
        [ testCase "Can serialize document with section and multiple kv pairs" <| fun () ->
              let result =
                  [ kvPair "TestStr" "Wowie"
                    kvPair "TestInt" "1" ]
                  |> section "TestSection"
                  |> fileOne
                  |> serialI

              let expected = [ "[TestSection]"; "TestStr=Wowie"; "TestInt=1" ] |> buildINIString
              Expect.equal result expected ""
          testCase "Can serialize document with multiple sections" <| fun () ->
              let result =
                  let section1 =
                      [ kvPair "TestStr" "Wowie"
                        kvPair "TestInt" "1" ]
                      |> section "TestSection"

                  let section2 =
                      [ kvPair "TestStr" "Wowza"
                        kvPair "TestInt2" "2" ]
                      |> section "TestSection2"

                  INIValue.File([ section1; section2 ]) |> serialI

              let expected =
                  [ "[TestSection]"; "TestStr=Wowie"; "TestInt=1\n"; "[TestSection2]"; "TestStr=Wowza"; "TestInt2=2" ]
                  |> buildINIString
              Expect.equal result expected ""
          testCase "Can serialize document with nested sections" <| fun () ->
              let result =
                  fieldText "TextField" "tfValue"
                  |> kvPairNested "Name"
                  |> kvPairNestedOne "TestStr"
                  |> sectionOne "TestSection"
                  |> fileOne
                  |> serialI

              let expected = [ "[TestSection]"; "TestStr=(Name=TextField(\"tfValue\"))" ] |> buildINIString
              Expect.equal result expected ""
          testCase "Can serialize document with empty kv pair" <| fun () ->
              let result =
                  INIValue.KeyValue("TestStr", INIValue.String(None))
                  |> sectionOne "TestSection"
                  |> fileOne
                  |> serialI

              let expected = [ "[TestSection]"; "TestStr=" ] |> buildINIString
              Expect.equal result expected "" ]
        |> testList "INI Serialization"

    let casting =
        [ testCase "Can parse strings" <| fun () ->
              let result =
                  let iVal = iStr "string"
                  iVal?File?TestSection?TestStr.AsString()

              let expected = "string"
              Expect.equal result expected ""
          testCase "Can parse booleans" <| fun () ->
              let result =
                  let iVal = iStr "true"
                  iVal?File?TestSection?TestStr.AsBoolean()

              let expected = true
              Expect.equal result expected ""
          testCase "Can parse integers" <| fun () ->
              let result =
                  let iVal = iStr "30"
                  iVal?File?TestSection?TestStr.AsInteger()

              let expected = 30
              Expect.equal result expected ""
          testCase "Can parse 64 bit integers" <| fun () ->
              let result =
                  let iVal = iStr "9223372036854775807"
                  iVal?File?TestSection?TestStr.AsInteger64()

              let expected = 9223372036854775807L
              Expect.equal result expected ""
          testCase "Can parse decimals" <| fun () ->
              let result =
                  let iVal = iStr "0.00000000000000001"
                  iVal?File?TestSection?TestStr.AsDecimal()

              let expected = 0.00000000000000001m
              Expect.equal result expected ""
          testCase "Can parse floats" <| fun () ->
              let result =
                  let iVal = iStr "1.1"
                  iVal?File?TestSection?TestStr.AsFloat()

              let expected = 1.1
              Expect.equal result expected "" ]
        |> testList "INI Casting"

    let operations =
        [ testCase "Can get inner text" <| fun () ->
              let result =
                  let iVal = iStr "wowie"
                  iVal.InnerText()

              let expected = "wowie"
              Expect.equal result expected ""
          testCase "Can map kv pair" <| fun () ->
              let result =
                  let oVal = iStr "wowie"
                  let nVal = iString "newWowie"
                  OptionTests.map oVal iStrSelectors nVal

              let expected = iStr "newWowie"
              Expect.equal result expected "" ]
        |> testList "INI Operations"

    [<Tests>]
    let tests = testList "INIValue" [ parsing; serializing; casting; operations ]
