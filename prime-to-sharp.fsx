type Export = {
    name: string
    extends: string option
}

let getRegexValueAtIndex index (regexCollection: System.Text.RegularExpressions.Match) =
    regexCollection.Groups
        |> Seq.cast<System.Text.RegularExpressions.Group>
        |> Seq.map (fun li -> li.Value)
        |> Seq.item index

let capitalize (s: string) =
    let head = s |> Seq.head |> string |> fun c -> c.ToUpper()
    let rest = s |> Seq.skip 1 |> Seq.map string |> String.concat ""

    sprintf "%s%s" head rest

let makePascalCase (propertyName: string) =
    propertyName.Split('_')
        |> Seq.map(capitalize)
        |> String.concat ""

let rec convertType (tsType: string) =
    match tsType with
    | "number" -> "long?"
    | "boolean" -> "boolean?"
    | "string" -> "string"
    | arrayType when arrayType.Contains "[]" ->
        let convertedType =
            arrayType
            |> (System.Text.RegularExpressions.Regex "\[\]").Split
            |> Seq.head
            |> convertType
        sprintf "IEnumerable<%s>" <| convertedType.Replace ("?", "")
    | s when s.Contains "|" -> sprintf "EnumBoy<%s>" s
    | t -> t

let trim (s: System.String) = s.Trim()

let rec extractNextComment (lines: string list) =
    // TODO: If the next non-empty line isn't a comment, return the lines given.
    [""]

let text = System.IO.File.ReadAllLines("PriceRule.ts");

// Find the interface
let interfaceNames =
    let getInterface = System.Text.RegularExpressions.Regex "export interface (.*) "
    let getBaseType = System.Text.RegularExpressions.Regex " extends (\w*)"
    text
        |> Seq.filter(getInterface.IsMatch)
        |> Seq.map(fun line ->
            let fullInterface =
                getInterface.Match line
                |> getRegexValueAtIndex 1
            let name =
                fullInterface.Split ' '
                |> Seq.head
            let extends = getBaseType.Match fullInterface

            // TODO: Parse all props within the interface
            // Will need to discover the end of the interface, so it will need
            // to keep track of opening curly brackets to match them with their
            // closing brackets.

            {
                name = name
                extends =
                    match extends.Success with
                    | true -> Some <| getRegexValueAtIndex 1 extends
                    | false -> None
            })

let sharpedProps =
    let isProperty = System.Text.RegularExpressions.Regex "\w*\?? ?:"
    let propNameSplitter = System.Text.RegularExpressions.Regex "\?? *:"
    let isNullable = System.Text.RegularExpressions.Regex "\?$"
    text
        |> Seq.filter(isProperty.IsMatch)
        |> Seq.map(fun line ->
            // This won't currently handle multi-line properties. For that we'd need curly bracket tracking.
            // Also will be messed up by colons in comments, for which we'd need comment tracking.
            let sides = propNameSplitter.Split line
            let propType =
                sides
                |> Seq.item 1
                |> trim
                |> convertType
            let propName =
                match sides |> Seq.head |> trim with
                | s when isNullable.IsMatch s -> isNullable.Replace(s, "")
                | s -> s

            [
                sprintf "[JsonProperty(\"%s\")]" propName
                sprintf "public %s %s { get; set; }" propType <| makePascalCase propName
            ])

// Need to create a function that will traverse the lines in a TypeScript documentation.
// It should be able to take one line, determine what type of line it is (empty, interface declaration, comment, property)
// and pass it to a dedicated line type parser function which can take multiple lines from the list, returning the parsed
// type + the remaining lines.
// Not sure if that's possible in F# though, wouldn't that imply a mutabe list?

let getComment (line: string): string option =
    let isBeginningComment = System.Text.RegularExpressions.Regex "^/\*"
    let isEndingComment = System.Text.RegularExpressions.Regex "^\*/$"
    let isComment = System.Text.RegularExpressions.Regex "^\*"

    match line.Trim() with
    | c when isBeginningComment.IsMatch c ->
        isBeginningComment.Split c |> Seq.item 1 |> Some
    | c when isEndingComment.IsMatch c ->
        isEndingComment.Split c |> Seq.item 1 |> Some
    | c when isComment.IsMatch c ->
        isComment.Split c |> Seq.item 1 |> Some
    | _ -> None

let comments =
    text |> Seq.map getComment |> Seq.filter Option.isSome |> Seq.map (fun i -> i.Value)

interfaceNames
    |> Seq.iter(fun tsInterface ->
        match tsInterface.extends with
        | Some extends -> printfn "public class %s : %s {" tsInterface.name extends
        | None -> printfn "public class %s {" tsInterface.name

        sharpedProps
            |> Seq.iter(fun prop ->
                prop
                    |> Seq.iter(fun propLine -> printfn "\t%s" propLine)

                printfn ""
            )

        comments |> Seq.iter (fun comment -> printfn "%s" comment)

        printfn "}")