(*** hide ***)
#I "../../bin"

(**
# What is MordhauBuddy? #

let ast =
    match File.Exists(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini") with
    | true ->
        //File.ReadAllTextAsync(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini")
        //|> Async.AwaitTask
        //|> Async.RunSynchronously
        //|> INIValue.TryParse
        File.ReadAllText(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini") |> INIValue.Parse
    | false -> failwith "Unable to parse file."

let result = ast.GetProperty(@"/Script/Mordhau.MordhauGameSession") |> List.tail |> List.tail |> List.head |> (fun r -> r.AsList())
let result2 =
    ast.Map
        ([ @"/Script/Mordhau.MordhauGameSession"; "ServerName"; "Server" ],
         INIValue.String(Some("\"332 Bardiche Test\"")))

let result3 =
    ast.Map
        ([@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"; "Name";"INVTEXT";"\"332 Bardiche\""],
         INIValue.String(Some("\"332 Bardiche Test\"")))

let result4 =
    ast.Map
        ([@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"; "FaceCustomization"; "Translate"; "15360"],
         INIValue.String(Some("00000")))

File.WriteAllText(@"C:\temp\test.ini", ast.ToString())
File.WriteAllText(@"C:\temp\test2.ini", result2.ToString())
File.WriteAllText(@"C:\temp\test3.ini", result3.ToString())
File.WriteAllText(@"C:\temp\test4.ini", result4.ToString())

## How to get MordhauBuddy ##

fill in

## Documentation ##

There is documentation on the structure of the library as well as how to use it:

 - Class/Type Documentation
   - fill in
 - Tutorials - Some example use cases for the application.
 - [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the functions.
 
## Contributing ##

The project is hosted on [Github][gh] where you can [report issues][issues], fork 
the project and submit pull requests. Please take a look at the [contribution guidelines](contributing.html)
for more information.

  [gh]: https://fillmein
  [issues]: https://fillmein
  [readme]: https://fillmein
  
*)
