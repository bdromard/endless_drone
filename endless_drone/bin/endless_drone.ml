open Yocaml

let template file = add_extension file "html" |> into "templates"
let article_template = template "article"
let review_template = template "review"
let layout = template "layout"

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue int)
        (Unix.getpid ()) Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let destination = "_build"
let css_destination = into destination "css"
let images_destination = into destination "images"
let track_binary_update = Build.watch Sys.argv.(0)


let may_process_markdown file =
  let open Build in
  if with_extension "md" file 
  then Yocaml_markdown.content_to_html ()
  else arrow Fun.id
;;

let pages =
  process_files [ "pages/" ] 
  (fun f -> with_extension "html" f || with_extension "md" f) 
  (fun file ->
    let fname = basename file |> into destination in
    let target = replace_extension fname "html" in
    let open Build in
    create_file 
      target 
      (track_binary_update
      >>> Yocaml_yaml.read_file_with_metadata (module Metadata.Page) file
      >>> may_process_markdown file
      >>> Yocaml_mustache.apply_as_template (module Metadata.Page) "templates/layout.html"
      >>^ Stdlib.snd))
;;

let article_destination file =
  let fname = basename file |> into "articles" in
  replace_extension fname "html"
;;

let articles =
  process_files [ "articles/" ] (with_extension "md") (fun file ->
    let open Build in
    let target = article_destination file |> into destination in
  create_file
    target (
    track_binary_update
    >>> Yocaml_yaml.read_file_with_metadata (module Metadata.Article) file
    >>> Yocaml_markdown.content_to_html ()  
    >>> Yocaml_mustache.apply_as_template (module Metadata.Article) article_template
    >>> Yocaml_mustache.apply_as_template (module Metadata.Article) layout
    >>^ Stdlib.snd))
;;

let book_reviews =
  process_files [ "articles/reviews" ] (with_extension "md") (fun file ->
    let open Build in
    let target = article_destination file |> into destination in
  create_file
    target (
    track_binary_update
    >>> Yocaml_yaml.read_file_with_metadata (module Model.Review) file
    >>> Yocaml_markdown.content_to_html ()  
    >>> Yocaml_mustache.apply_as_template (module Model.Review) review_template
    >>> Yocaml_mustache.apply_as_template (module Model.Review) layout
    >>^ Stdlib.snd))
;;

let css =
  process_files [ "css/" ] (with_extension "css") (fun file ->
  Build.copy_file file ~into:css_destination)
;;

let images =
  process_files [ "images/" ] (fun f ->
  with_extension "svg" f
  || with_extension "jpg" f
  || with_extension "png" f
  || with_extension "gif" f)
  (fun file -> Build.copy_file file ~into:images_destination)
;;

let index =
  let open Build in
  let* articles = 
    collection
      (collect_child_files ["articles"; "articles/reviews"] (with_extension "md"))
      (fun source ->
        track_binary_update
        >>> Yocaml_yaml.read_file_with_metadata (module Metadata.Article) source
        >>^ fun (x, _) -> x, article_destination source)
        (fun x (meta, content) -> 
         x
         |> Metadata.Articles.make
              ?title:(Metadata.Page.title meta)
              ?description:(Metadata.Page.description meta)
         |> Metadata.Articles.sort_articles_by_date
         |> fun x -> x, content)
  in
  create_file
    (into destination "index.html")
    (track_binary_update
    >>> Yocaml_yaml.read_file_with_metadata (module Metadata.Page) "pages/index.md"
    >>> Yocaml_markdown.content_to_html ()
    >>> articles
    >>> Yocaml_mustache.apply_as_template (module Metadata.Articles) "templates/layout.html"
    >>> Yocaml_mustache.apply_as_template (module Metadata.Articles) "templates/articles_list.html"
    >>^ Stdlib.snd)
;;

let () =
  Yocaml_unix.execute (pages >> css >> images >> articles >> book_reviews >> index)
