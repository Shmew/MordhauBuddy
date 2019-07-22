namespace Tools.Electron

open System

[<RequireQualifiedAccess>]
module Json =
    open FSharp.Json
    open Fake.IO
    open Fake.IO.FileSystemOperators

    type Scripts = {
        Dev: string
        Compile: string
        Dist: string
        [<JsonField("dist:dir")>]
        DistDir: string
    }

    type ProjectsWebpack = {
        SourceDirectory: string
        WebpackConfig: string
    }

    type ElectronWebpack = {
        Title: bool
        Main: ProjectsWebpack
        Renderer: ProjectsWebpack
    }

    type JsonPackage = {
        Name: string
        Version: string
        Scripts: Scripts
        ElectronWebpack: ElectronWebpack
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

