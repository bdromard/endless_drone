open Yocaml

module Review : sig  
  type t

  val date : t -> Date.t
 
  include Metadata.INJECTABLE with type t := t
  include Metadata.READABLE with type t := t
end
