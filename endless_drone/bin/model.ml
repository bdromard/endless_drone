open Yocaml 

module Reference = struct
  type t = 
    { author : string ;
      title : string ;
      publisher : string ;
      publication_year : int ;
      cover : string ;
      cover_alt : string ;
    }

  let make author title publisher publication_year cover cover_alt =
    { author ; title ; publisher ; publication_year ; cover ; cover_alt }

  
  let from (type a) (module V : Metadata.VALIDABLE with type t = a) obj =
    V.object_and 
    (fun assoc ->
            let open Validate.Applicative in
            make
            <$> V.(required_assoc string) "author" assoc
            <*> V.(required_assoc string) "title" assoc
            <*> V.(required_assoc string) "publisher" assoc
            <*> V.(required_assoc integer) "publication_year" assoc
            <*> V.(required_assoc string) "cover" assoc
            <*> V.(required_assoc string) "cover_alt" assoc)
    obj
  ;;

  let inject 
    (type a)
    (module D : Key_value.DESCRIBABLE with type t = a)
    { author ; title ; publisher ; publication_year ; cover ; cover_alt }
    =
    D.
      [ "author", string author ;
        "title", string title ;
        "publisher", string publisher ;
        "publication_year", integer publication_year ;
        "cover", string cover ;
        "cover_alt", string cover_alt
      ]
  ;;
end

module Review = struct
  type t =
    { article_title : string
    ; article_description : string 
    ; date : Date.t
    ; reference : Reference.t
    }

  let date { date; _} = date
  
  let make article_title article_description date reference =
    { article_title 
    ; article_description 
    ; date 
    ; reference 
    }
  ;;

  let from_string (module V : Metadata.VALIDABLE) = function
    | None -> Validate.error $ Error.Required_metadata [ "Review" ]
    | Some str ->
      let open Validate.Monad in
      V.from_string str 
      >>= V.object_and (fun assoc ->
            let open Validate.Applicative in
            make
            <$> V.(required_assoc string) "article_title" assoc
            <*> V.(required_assoc string) "article_description" assoc
            <*> V.required_assoc(Metadata.Date.from (module V)) "date" assoc
            <*> V.(required_assoc (Reference.from (module V))) "reference" assoc)
  ;;

  let inject 
  (type a)
  (module D : Key_value.DESCRIBABLE with type t = a) 
  { article_title ; article_description ; date ; reference } = 
  D.
  [ "article_title", string article_title
    ; "article_description", string article_description 
    ; "date", object_ $ Metadata.Date.inject (module D) date
    ; "reference", object_ $ Reference.inject (module D) reference
  ]
;;

end

