namespace Tools.Electron

open System

[<RequireQualifiedAccess>]
module Json =
    open FSharp.Json
    open Fake.IO
    open Fake.IO.FileSystemOperators

    type ProjectsWebpack = {
        SourceDirectory: string
        WebpackConfig: string
    }

    type ElectronWebpack = {
        Title: bool
        Main: ProjectsWebpack
        Renderer: ProjectsWebpack
    }

    type Repo = {
        Type: string
        Url: string
    }

    type Bugs = {
        Url: string
    }

    [<NoComparison>]
    type JsonPackage = {
        Name: string
        Description: string
        Version: string
        Scripts: obj
        Repository: Repo
        Author: string
        License: string
        Bugs: Bugs
        Homepage: string
        ElectronWebpack: ElectronWebpack
        Build: obj
        Dependencies: obj
        DevDependencies: obj
    }

    let config = JsonConfig.create(jsonFieldNaming = Json.lowerCamelCase,allowUntyped = true)

    let jsonPath = (__SOURCE_DIRECTORY__ @@ "../package.json")

    let getJsonPkg () = 
        File.readAsString jsonPath
        |> Json.deserializeEx<JsonPackage> config

    let setJsonPkg (f: JsonPackage -> JsonPackage) =
        getJsonPkg()
        |> f
        |> Json.serializeEx config
        |> File.writeString false jsonPath

module Str =
    open System.Text.RegularExpressions

    let setLowerFirst (s: string) = 
        sprintf "%s%s" (s.Substring(0,1).ToLower()) (s.Substring(1))

    let toKebabCase (s: string) =
        MatchEvaluator(fun m -> "-" + m.Value.ToLower())
        |> (fun m -> Regex.Replace(setLowerFirst s,"[A-Z]",m))
