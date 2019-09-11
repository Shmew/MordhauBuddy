namespace MordhauBuddy.Core

open System
open System.Xml
open System.Xml.Linq
open MordhauBuddy.Shared.ElectronBridge

/// Module for community related content
module ComOperations =
    module private XML =
        let tryParse s =
            try
                XDocument.Parse s |> Some
            with _ -> None

        let xn s = XName.Get s

        let tryHeadValue (elemSeq: Collections.Generic.IEnumerable<XElement>) =
            elemSeq
            |> Seq.tryHead
            |> Option.map (fun xElem -> xElem.Value)

        let removeDuplicates (aOpts: (string * string) list option) =
            aOpts
            |> Option.map (fun (aList: (string * string) list) ->
                aList
                |> List.windowed 2
                |> List.choose (fun wList ->
                    match wList with
                    | [ l1; l2 ] when wList.Length = 2 ->
                        match l1, l2 with
                        | (t1, b1), (t2, b2) when t1 = t2 ->
                            if b1.Length > b2.Length then [ (t1, b1) ]
                            else [ (t2, b2) ]
                            |> Some
                        | _ -> [ l1; l2 ] |> Some
                    | _ -> None))
            |> Option.map (List.concat >> List.distinct)

        /// Parse the `XDocument` into a consumable format
        let getXDocAnnouncements (xDoc: XDocument) =
            xDoc.Root.Elements(xn "channel")
            |> Seq.choose (fun xElem ->
                xElem.Elements(xn "item")
                |> Seq.map (fun xElem ->
                    let title = xElem.Elements(xn "title") |> tryHeadValue
                    let desc = xElem.Elements(xn "description") |> tryHeadValue
                    title, desc)
                |> List.ofSeq
                |> List.choose (fun (tOpt, dOpt) ->
                    match tOpt, dOpt with
                    | Some(title), Some(desc) -> Some(title, desc)
                    | _ -> None)
                |> function
                | xList when xList.Length > 0 -> Some(xList)
                | _ -> None)
            |> Seq.tryHead
            |> removeDuplicates

    /// Get announcements from Steam Mordhau rss feed
    let getAnnouncements s =
        XML.tryParse s
        |> Option.map XML.getXDocAnnouncements
        |> Option.flatten
