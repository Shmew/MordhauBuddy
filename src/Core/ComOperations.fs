namespace MordhauBuddy.Core

open System
open System.Xml
open System.Xml.Linq
open MordhauBuddy.Shared.ElectronBridge

/// Module for community related content
module ComOperations =
    module private XML =
        let tryParse s = 
            try XDocument.Parse s |> Some
            with | _ -> None

        let xn s = XName.Get s
        let tryHeadValue (elemSeq: Collections.Generic.IEnumerable<XElement>) =
            elemSeq
            |> Seq.tryHead
            |> Option.map (fun xElem -> xElem.Value)

        let getXDocAnnouncements (xDoc: XDocument) =
            xDoc.Root.Elements(xn "channel") 
            |> Seq.choose (fun xElem -> 
                xElem.Elements(xn "item") 
                |> Seq.map (fun xElem -> 
                    let title =
                        xElem.Elements(xn "title")
                        |> tryHeadValue
                    let desc = 
                        xElem.Elements(xn "description")
                        |> tryHeadValue
                    title,desc)
                |> List.ofSeq
                |> List.choose (fun (tOpt, dOpt) -> 
                    match tOpt, dOpt with
                    | Some(title), Some(desc) -> Some(title,desc)
                    | _ -> None)
                |> function 
                | xList when xList.Length > 0 -> Some(xList)
                | _ -> None)
            |> Seq.tryHead

    let getAnnouncements s =
        XML.tryParse s
        |> Option.map XML.getXDocAnnouncements
        |> Option.flatten