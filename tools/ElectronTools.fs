namespace Tools.Electron

open System

module JsonPackage =
    open FSharp.Json

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

    let test = Json.deserializeEx<JsonPackage> config (System.IO.File.ReadAllText(@"C:\Users\shmew\source\repos\MordhauBuddy\package.json"))
    let test2 = Json.serializeEx config test
    
    System.IO.File.WriteAllText(@"C:\Users\shmew\source\repos\MordhauBuddy\packageTest.json",test2)
