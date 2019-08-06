namespace MordhauBuddy.Core.Tests

module INIConfiguration =
    open Expecto
    open MordhauBuddy.Core
    open INIReader
    open INIReader.INIExtensions
    open INIConfiguration
    open Frankenstein
    open System

    [<AutoOpen>]
    module Utils =
        let gameFile = 
            IO.File.ReadAllText("tests/MordhauBuddy.Core.Tests/Data/Game.ini")
            |> INIValue.Parse

        let characterProps =
            [gameFile]
            |> mapProps "File"
            |> mapProps @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"
            |> List.map (fun iVal ->
                match iVal.TryGetProperty("CharacterProfiles") with
                | Some(iList) -> 
                    iList 
                | None -> [])
            |> List.filter (List.isEmpty >> not)
    
        let characterProp =
            characterProps
            |> List.head
            |> fun i -> INIValue.KeyValue("CharacterProfile",INIValue.Tuple(i))

        let profileNames =
            [ "332 Bardiche"; "333 Greatsword"; "332 Eveningstar"; "333 Poleaxe t"; "231 Spear"; "231 Spear goofy"; "333 Maul"; "333 messer shield"; "333 Exec"; "010 Exec bl rush"; "333 Bastard Buckler"; "221 Halberd"; "333 Battle Axe t"; "333 War Axe sw"; "111 Halberd Bandage a"]

        let bardExport =
            @"(Translate=(15360,15360,15840,12656,15364,12653,15862,0,15385,0,16320,15847,15855,15855,384,8690,8683,480,480,480,31700,480,480,480,15360,15840,18144,15840,31690,15850,15860,11471,11471,12463,12463,11471,11471,15840,15840,0,0,0,0,0,0,0,0,7665,7660),Rotate=(0,0,0,0,0,0,16,0,0,0,0,14,0,0,12288,591,367,0,15855,15855,18976,0,0,0,0,18432,0,0,18816,0,0,0,0,0,0,0,0,655,335,0,0,15840,15840,0,0,15840,15840,0,0),Scale=(14351,14351,0,15360,0,15360,0,15855,0,15855,14336,0,0,0,14350,0,0,15855,15855,15855,15840,0,15855,15855,0,15914,6,0,15840,0,0,0,0,0,0,0,0,15855,15855,0,0,0,0,0,0,0,0,0,0))"

    let frankenstein =
        [ testCase "modifyFace changes character values" <| fun () ->
            let result =
                modifyFace characterProp randomFaces
                |> string
            Expect.notEqual result (characterProp.ToString()) ""
          testCase "getCharacterProfileNames returns a list of profile names" <| fun () ->
            let result =
                getCharacterProfileNames gameFile
            Expect.equal result profileNames ""
          testCase "getCharacterProfileExports returns a list of profile names and export strings" <| fun () ->
            let result = getCharacterProfileExports gameFile ["332 Bardiche"] |> List.head
            let expected = ("332 Bardiche", bardExport)
            Expect.equal result expected ""
          testCase "modifyCharacterProfileFace modifies the character profile face" <| fun () ->
            let result =
                modifyCharacterProfileFace gameFile "332 Bardiche" FaceActions.Frankenstein
                |> string
            let expected = bardExport
            Expect.notEqual result expected ""
        ]
        |> testList "Frankenstein"

    [<Tests>]
    let tests = testList "INIValue" [ frankenstein ]
