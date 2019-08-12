namespace MordhauBuddy.Core.Tests

module INIConfiguration =
    open Expecto
    open MordhauBuddy.Core
    open INIReader
    open INIConfiguration
    open Frankenstein
    open System

    [<AutoOpen>]
    module Utils =
        let gameFile = IO.File.ReadAllText("tests/MordhauBuddy.Core.Tests/Data/Game.ini") |> INIValue.Parse
        let profileNames =
            [ "332 Bardiche"; "333 Greatsword"; "332 Eveningstar"; "333 Poleaxe t"; "231 Spear"; "231 Spear goofy";
              "333 Maul"; "333 messer shield"; "333 Exec"; "010 Exec bl rush"; "333 Bastard Buckler"; "221 Halberd";
              "333 Battle Axe t"; "333 War Axe sw"; "111 Halberd Bandage a" ]
        let bardExport =
            "(Translate=(15360,15360,15840,12656,15364,12653,15862,0,15385,0,16320,15847,15855,15855,384,8690,8683,480,480,480,31700,480,480,480,15360,15840,18144,15840,31690,15850,15860,11471,11471,12463,12463,11471,11471,15840,15840,0,0,0,0,0,0,0,0,7665,7660),Rotate=(0,0,0,0,0,0,16,0,0,0,0,14,0,0,12288,591,367,0,15855,15855,18976,0,0,0,0,18432,0,0,18816,0,0,0,0,0,0,0,0,655,335,0,0,15840,15840,0,0,15840,15840,0,0),Scale=(14351,14351,0,15360,0,15360,0,15855,0,15855,14336,0,0,0,14350,0,0,15855,15855,15855,15840,0,15855,15855,0,15914,6,0,15840,0,0,0,0,0,0,0,0,15855,15855,0,0,0,0,0,0,0,0,0,0))"
        let faceImport =
            "(Translate=(65535,31072,875,704,0,734,65535,0,0,65535,31565,0,0,65535,0,29632,29662,30686,65535,65535,0,30720,65535,0,0,918,31560,0,65535,31709,31680,544,574,30749,30720,256,286,65535,0,0,0,65535,0,0,65535,0,65535,31678,31648),Rotate=(0,65535,26302,31680,0,30750,0,65535,0,0,0,65535,65535,65535,31584,30749,31648,8,65535,0,65535,608,65535,65535,0,31695,893,18301,65535,31677,30720,31704,30725,1,988,29,960,0,65535,0,65535,65535,16326,0,65535,65535,15383,30,960),Scale=(0,30,4139,30749,65535,30749,0,65535,65535,0,0,0,0,65535,31709,0,0,190,0,0,0,589,0,0,0,30749,31166,989,65535,5085,5085,4242,4242,0,0,24452,24452,65535,0,0,65535,65535,574,0,0,65535,574,21470,21470))"

        let notOrigAndParses (strRes: string) =
            let original = gameFile |> string
            let parseRes = strRes |> INIValue.TryParse
            strRes <> original && parseRes.IsSome

    let frankenstein =
        [ testCase "getCharacterProfileNames returns a list of profile names" <| fun () ->
              let result = getCharacterProfileNames gameFile
              Expect.equal result profileNames ""
          testCase "getCharacterProfileExports returns a list of profile names and export strings" <| fun () ->
              let result = getCharacterProfileExports gameFile [ "332 Bardiche" ] |> List.head
              let expected = ("332 Bardiche", bardExport)
              Expect.equal result expected ""
          testCase "setCharacterProfileFace handles frankenstein action" <| fun () ->
              setCharacterProfileFace gameFile "332 Bardiche" FaceActions.Frankenstein
              |> string
              |> notOrigAndParses
              |> Expect.isTrue
              <| ""
          testCase "setCharacterProfileFace handles random action" <| fun () ->
              setCharacterProfileFace gameFile "332 Bardiche" FaceActions.Random
              |> string
              |> notOrigAndParses
              |> Expect.isTrue
              <| ""
          testCase "setCharacterProfileFace handles custom import string action" <| fun () ->
              FaceActions.Custom(faceImport)
              |> setCharacterProfileFace gameFile "332 Bardiche"
              |> string
              |> notOrigAndParses
              |> Expect.isTrue
              <| "" ]
        |> testList "Frankenstein"

    [<Tests>]
    let tests = testList "INIValue" [ frankenstein ]
