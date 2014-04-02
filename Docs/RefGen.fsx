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
  File.ReadAllLines file
  |> Array.map trim
  |> List.ofArray

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
  if s.StartsWith "/// " then Some (s.Substring 4) else None

let (|Attr|_|) (s: string) = if isAttr s then Some s else None

let (|Punc|_|) (s: string) =
  let ss = summarize s
  match ss with
   | "{"::_ | "}"::_ | "->"::_ -> Some ss
   | _ -> None

let (|Def|_|) (s: string) =
  let ss = summarize s
  match ss with
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
  let rec outside indent items docs attrs = function
     | [] -> (List.rev items, [])
     | (_, Ignored ())::lines ->
       outside indent items docs attrs lines
     | (i, Punc text)::lines ->
       outside indent ({empty with Text = text; Indent = i}::items) [] [] lines
     | lines & (indent', _)::_ when indent' <= indent ->
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

let asText (s: string) =
  let b = StringBuilder ()
  for i=0 to s.Length-1 do
    match s.[i] with
      | '<' -> b.Append "&lt;" |> ignore
      | '>' -> b.Append "&gt;" |> ignore
      | '&' -> b.Append "&amp;" |> ignore
      | c   -> b.Append c |> ignore
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

let printElem wr elem =
  match elem with
   | "member" | "module" | "namespace" | "new" | "static" | "type" | "val"
   | "(" | ")" | "->" | "{" | "}" | ":" ->
     fprintf wr "<b>%s</b>" (asText elem)
   | _ ->
     fprintf wr "%s" (asText elem)

let printText wr path inSection toSection item =
  let linked = ref false
  let printLink elem =
    if not (!linked) && item.Doc.Length > 0 && Some elem = item.Name then
      linked := true
      let text = asText elem
      let link = asText (String.concat "." (List.rev (elem::path)))
      let id = match inSection with
                | None -> " "
                | Some inSection -> sprintf " id=\"%s:%s\" " inSection link
      fprintf wr "<a%shref=\"#%s:%s\">%s</a>" id toSection link text
    else
      printElem wr elem
  let rec loop space = function
     | "("::elem::")"::":"::elems ->
       if space then fprintf wr " "
       printElem wr "("
       printLink elem
       printElem wr ")"
       printElem wr ":"
       loop true elems
     | elem::":"::elems ->
       if space then fprintf wr " "
       printLink elem
       printElem wr ":"
       loop true elems
     | elem::"="::elems ->
       if space then fprintf wr " "
       printLink elem
       printElem wr " ="
       loop true elems
     | elem::elems ->
       if space then fprintf wr " "
       printLink elem
       loop true elems
     | [] ->
       fprintf wr "\n"
  loop false item.Text

let rec printSummary wr deep path inSection toSection drop item =
  let indent = max (item.Indent - drop) 0
  let prefix = String.replicate indent " "
  let print s = fprintf wr "%s%s\n" prefix (asText s)
  item.Attr
  |> Seq.iter (fun attr ->
     fprintf wr "%s<b>%s</b>%s<b>%s</b>\n"
      <| prefix
      <| asText "[<"
      <| asText (attr.Substring (2, attr.Length-4))
      <| asText ">]")
  fprintf wr "%s" prefix
  printText wr path inSection toSection item
  let path =
    match item.Name with
     | None -> path
     | Some name -> name::path
  if deep then
    item.Body
    |> Seq.iter (printSummary wr deep path inSection toSection drop)
  else
    if not (List.exists (fun item -> Option.isSome item.Name) item.Body) then
      item.Body
      |> Seq.filter (fun item ->
         match item.Text with
          | "{"::_ | "}"::_ -> false
          | _ -> true)
      |> Seq.iter (printSummary wr false path inSection toSection drop)

let rec printDescription wr path item =
  if Option.isSome item.Name &&
     (not (List.isEmpty item.Doc) ||
      not (List.isEmpty item.Body)) then
    fprintf wr "<pre>"
    printSummary wr false path (Some "def") "dec" item.Indent item
    fprintf wr "</pre>\n"
    fprintf wr "<blockquote>"
    match item.Doc with
     | [] -> ()
     | lines ->
       fprintf wr "<p>%s</p>\n" (String.concat " " lines |> formatDesc)
    match item.Body with
     | [] -> ()
     | body ->
       let path =
         match item.Name with
          | None -> path
          | Some name -> name::path
       body
       |> Seq.iter (printDescription wr path)
    fprintf wr "</blockquote>\n"

let generate wr title path =
  let units =
    Directory.EnumerateFiles (path, "*.fsi")
    |> Seq.map (input >> itemize)
    |> List.ofSeq
  let model =
    let collect field =
      units
      |> List.collect (fun items ->
         items |> List.collect field)
    match units with
     | (unit::_)::_ ->
       {unit with
         Doc = collect (fun item -> item.Doc)
         Attr = collect (fun item -> item.Attr)
         Body = collect (fun item -> item.Body)}
     | _ -> failwith "Expected some!"
  fprintf wr "<!DOCTYPE html>\n\
              <html>\n"
  fprintf wr "<head>\n"
  fprintf wr "<title>%s Library Reference</title>\n" title
  fprintf wr "<style>\n\
pre {\n\
  border: 1px solid #e0e0e0;\n\
  border-radius: 3px;\n\
  padding: 5px;\n\
  line-height: 160%%;\n\
  background: #f7f7f7;\n\
  font-family: \"Lucida Console\", Monaco, monospace;\n\
  font-size: 72%%;\n\
}\n\
code {\n\
  border: 1px solid #e0e0e0;\n\
  border-radius: 3px;\n\
  padding: 2px;\n\
  background: #f7f7f7;\n\
  font-family: \"Lucida Console\", Monaco, monospace;\n\
  font-size: 72%%;\n\
}\n\
a {\n\
  text-decoration: none;\n\
  font-weight: bold;\n\
}\n\
</style>\n"
  fprintf wr "</head>\n"
  fprintf wr "<body><table width=\"80%%\" align=\"center\"><tr><td>\n"
  fprintf wr "<h1>%s Library Reference</h1>\n\
              <p>This document provides a reference manual for the %s library \
                 and is generated from the library source code.</p>\n" title title
  fprintf wr "<h2>Synopsis</h2>\n"
  fprintf wr "<pre>"
  printText wr [] (Some "dec") "def" model
  fprintf wr "</pre>\n"
  model.Body
  |> Seq.iter (fun item ->
     fprintf wr "<pre>"
     printSummary wr true [Option.get model.Name] (Some "dec") "def" 0 item
     fprintf wr "</pre>\n")
  fprintf wr "<h2>Description</h2>\n"
  printDescription wr [] model
  fprintf wr "</td></tr></table></body>\n"
  fprintf wr "</html>\n"

do use wr = new StreamWriter ("Hopac.html")
   generate wr "Hopac" "../Libs/Hopac"

do use wr = new StreamWriter ("Hopac.Extra.html")
   generate wr "Hopac.Extra" "../Libs/Hopac.Extra"
