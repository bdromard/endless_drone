open Yocaml 

module Reference = struct
  type t = 
    { ref_author : string ;
      ref_title : string ;
      ref_publisher : string ;
      ref_publication_year : int ;
      ref_cover : string 
    }

  let make ref_author ref_title ref_publisher ref_publication_year ref_cover =
    { ref_author ; ref_title ; ref_publisher ; ref_publication_year ; ref_cover }

  
  let from (type a) (module V : Metadata.VALIDABLE with type t = a) obj =
    V.object_and 
    (fun assoc ->
            let open Validate.Applicative in
            make
            <$> V.(required_assoc string) "ref_author" assoc
            <*> V.(required_assoc string) "ref_title" assoc
            <*> V.(required_assoc string) "ref_publisher" assoc
            <*> V.(required_assoc integer) "ref_publication_year" assoc
            <*> V.(required_assoc string) "ref_cover" assoc)
    obj
  ;;

  let inject 
    (type a)
    (module D : Key_value.DESCRIBABLE with type t = a)
    { ref_author ; ref_title ; ref_publisher ; ref_publication_year ; ref_cover }
    =
    D.
      [ "ref_author", string ref_author ;
        "ref_title", string ref_title ;
        "ref_publisher", string ref_publisher ;
        "ref_publication_year", integer ref_publication_year ;
        "ref_cover", string ref_cover ;
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

