#light (*
  exec fsi --exec $0 --quiet
*)

open System
open System.IO
open System.Text

let indentOf (s: String) =
  let rec loop i = if i < s.Length && s.[i] = ' ' then loop (i+1) else i
  loop 0

let trim s = (indentOf s, s.Trim ())

let input (file: string) =
  use r = new StreamReader (file)
  let rec loop ls =
    match r.ReadLine () with
     | null -> List.rev ls
     | l -> loop (trim l::ls)
  loop []

type Item = {
  Doc: list<string>
  Attr: list<string>
  Name: option<string>
  Indent: int
  Text: list<string>
  Body: list<Item>
}
let empty = {
  Doc = []
  Attr = []
  Name = None
  Indent = 0
  Text = []
  Body = []
}

let isAttr (s: string) = s.StartsWith "[<" && s.EndsWith ">]"

let summarize (s: string) =
  s.Split ' '
  |> List.ofArray
  |> List.filter (fun s -> not (isAttr s || s = "inline"))
  |> List.collect (fun s ->
     if s.EndsWith ":" then [s.Substring (0, s.Length-1); ":"] else [s])
  |> List.collect (fun s ->
     if s.StartsWith "(" && s.EndsWith ")"
     then ["("; s.Substring (1, s.Length-2); ")"]
     else [s])

let (|Ignored|_|) (s: string) =
  if s.Length = 0 ||
     s.StartsWith "// " ||
     s.StartsWith "open "
  then Some ()
  else None

let (|Doc|_|) (s: string) =
  if s.StartsWith "/// " then Some (s.Substring (4)) else None

let (|Attr|_|) (s: string) = if isAttr s then Some s else None

let (|Punc|_|) (s: string) =
  let ss = summarize s
  match ss with
   | "{"::_ | "}"::_ | "->"::_ -> Some ss
   | _ -> None

let (|Def|_|) (s: string) =
  let ss = summarize s
  ss
  |> function
   | "module"          ::name::_
   | "namespace"       ::name::_
   | "type"            ::name::_
   | "member"          ::name::_
   | "static"::"member"::name::_
   | "val"        ::"("::name::")"::_
   | "val"             ::name::_ -> Some (name, ss)
   | "new"::_                    -> Some ("new", ss)
   | name :: ":" :: _            -> Some (name, ss)
   | _                           -> None

let (|Namespace|_|) (s: string) =
  let ss = summarize s
  match ss with
   | "namespace"::name::_ -> Some (name, ss)
   | _                    -> None

let rec itemize ls =
  let rec outside indent items docs attrs lines =
    match lines with
     | [] -> (List.rev items, [])
     | (_, Ignored ())::lines ->
       outside indent items docs attrs lines
     | (i, Punc text)::lines ->
       outside indent ({empty with Text = text; Indent = i}::items) [] [] lines
     | (indent', _)::_ when indent' <= indent ->
       (List.rev items, lines)
     | (0, Namespace (name, text))::lines ->
       let (body, lines) = outside indent [] [] [] lines
       let item = {
         Doc = List.rev docs
         Attr = List.rev attrs
         Name = Some name
         Indent = 0
         Text = text
         Body = body
       }
       outside indent (item::items) [] [] lines
     | (_, Attr attr)::lines -> outside indent items docs (attr::attrs) lines
     | (_, Doc doc)::lines -> outside indent items (doc::docs) attrs lines
     | (i, Def (name, text))::lines ->
       let (body, lines) = outside i [] [] [] lines
       let item = {
         Doc = List.rev docs
         Attr = List.rev attrs
         Name = Some name
         Indent = i
         Text = text
         Body = body
       }
       outside indent (item::items) [] [] lines
     | _::lines ->
       // Ignore unrecognized 
       outside indent items docs attrs lines
  match outside -1 [] [] [] ls with
   | (items, _) -> items

let model =
  input "Libs/Hopac/Hopac.fsi"
  |> itemize
  |> function
   | [item] -> item
   | items -> failwithf "Expected one item, but got %A" items

let asText (s: string) =
  let b = StringBuilder ()
  for i=0 to s.Length-1 do
    ignore <|
    match s.[i] with
      | '<' -> b.Append "&lt;"
      | '>' -> b.Append "&gt;"
      | '&' -> b.Append "&amp;"
      | c   -> b.Append c
  b.ToString ()

let formatDesc (text: string) =
  let b = StringBuilder ()
  let rec outside i =
    if i < text.Length then
      match text.[i] with
       | '"' ->
         b.Append "<code>" |> ignore
         inside (i+1)
       | c ->
         b.Append c |> ignore
         outside (i+1)
    else
      b.ToString ()
  and inside i =
    if i < text.Length then
      match text.[i] with
       | '"' ->
         b.Append "</code>" |> ignore
         outside (i+1)
       | '<' -> b.Append "&lt;"  |> ignore ; inside (i+1)
       | '>' -> b.Append "&gt;"  |> ignore ; inside (i+1)
       | '&' -> b.Append "&amp;" |> ignore ; inside (i+1)
       | c   -> b.Append c       |> ignore ; inside (i+1)
    else
      failwithf "Failed to escape text properly: %s" text
  outside 0

let printElem elem =
  match elem with
   | "member" | "module" | "namespace" | "new" | "static" | "type" | "val"
   | "(" | ")" | "->" | "{" | "}" | ":" ->
     printf "<b>%s</b>" (asText elem)
   | _ ->
     printf "%s" (asText elem)    

let printText path inSection toSection item =
  let linked = ref false
  let printLink elem =
    if not (!linked) && item.Doc.Length > 0 && Some elem = item.Name then
      linked := true
      let text = asText elem
      let link = asText (String.concat "." (List.rev (elem::path)))
      printf "<a id=\"%s:%s\" href=\"#%s:%s\">%s</a>" inSection link toSection link text
    else
      printElem elem
  let rec loop space elems =
    match elems with
     | "("::elem::")"::elems ->
       if space then printf " "
       printElem "("
       printLink elem
       printElem ")"
       loop true elems
     | elem::":"::elems ->
       if space then printf " "
       printLink elem
       printElem ":"
       loop true elems
     | elem::"="::elems ->
       if space then printf " "
       printLink elem
       printElem " ="
       loop true elems
     | elem::elems ->
       if space then printf " "
       printLink elem
       loop true elems
     | [] ->
       printf "\n"
  loop false item.Text

let rec printSummary deep path inSection toSection drop item =
  let indent = max (item.Indent - drop) 0
  let prefix = String.replicate indent " "
  let print s = printf "%s%s\n" prefix (asText s)
  item.Attr
  |> Seq.iter (fun attr ->
     printf "%s<b>%s</b>%s<b>%s</b>\n"
      <| prefix
      <| asText "[<"
      <| asText (attr.Substring (2, attr.Length-4))
      <| asText ">]")
  printf "%s" prefix
  printText path inSection toSection item
  let path =
    match item.Name with
     | None -> path
     | Some name -> name::path
  if deep then
    item.Body
    |> Seq.iter (printSummary deep path inSection toSection drop)
  else
    if not (List.exists (fun item -> Option.isSome item.Name) item.Body) then
      item.Body
      |> Seq.filter (fun item ->
         match item.Text with
          | "{"::_ | "}"::_ -> false
          | _ -> true)
      |> Seq.iter (printSummary false path inSection toSection drop)

let rec printDescription path item =
  if Option.isSome item.Name &&
     (not (List.isEmpty item.Doc) ||
      not (List.isEmpty item.Body)) then
    match item.Name with
     | None ->
       printf "<pre>"
     | Some name ->
       let link = List.rev (name::path) |> String.concat "." |> asText
       printf "<pre id=\"def:%s\">" link
    printSummary false path "" "dec" item.Indent item
    printf "</pre>\n"
    match item.Doc with
     | [] -> ()
     | lines ->
       printf "<blockquote>%s</blockquote>\n" (String.concat " " lines |> formatDesc)
    printf "<blockquote>\n"
    let path =
      match item.Name with
       | None -> path
       | Some name -> name::path
    item.Body
    |> Seq.iter (printDescription path)
    printf "</blockquote>\n"

do printf "<!DOCTYPE html>\n\
           <html>\n\
           <title>Hopac Library Reference</title>\n\
           <h1>Hopac Library Reference</h1>\n\
           <p>This document provides a reference manual of the Hopac library and is generated \
              from the library source code.</p>\n"
   printf "<h2>Synopsis</h2>\n"
   printf "<pre id=\"dec:%s\">" (Option.get model.Name)
   printText [] "dec" "def" model
   printf "</pre>\n"
   model.Body
   |> Seq.iter (fun item ->
      match item.Name with
       | None ->
         printf "<pre>"
       | Some name ->
         printf "<pre id=\"dec:%s\">" name
      printSummary true [Option.get model.Name] "dec" "def" 0 item
      printf "</pre>\n")
   printf "<h2>Description</h2>\n"
   printDescription [] model
   printf "</html>\n"
