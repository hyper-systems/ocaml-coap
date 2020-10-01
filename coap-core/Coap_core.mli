

type error = [
  | `Invalid_token_length
  | `Invalid_option_delta
  | `Invalid_option_length
]

val pp_error : Format.formatter -> error -> unit


module Message : sig
  type t

  type buffer = (
    char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout
  ) Bigarray_compat.Array1.t

  type kind =
    | Confirmable
    | Nonconfirmable
    | Acknowledgement
    | Reset

  type code =
    | Empty
    | Request of [
      | `Get
      | `Post
      | `Put
      | `Delete
    ]
    | Response of [
      (* 2XX *)
      | `Created
      | `Deleted
      | `Valid
      | `Changed
      | `Content

      (* 4XX *)
      | `Bad_request
      | `Unauthorized
      | `Bad_option
      | `Forbidden
      | `Not_found
      | `Method_not_allowed
      | `Not_acceptable
      | `Precondition_failed
      | `Request_entity_too_large
      | `Unsupported_content_format

      (* 5xx *)
      | `Internal_server_error
      | `Not_implemented
      | `Bad_gateway
      | `Service_unavailable
      | `Gateway_timeout
      | `Proxying_not_supported
    ]

  val pp_code : Format.formatter -> code -> unit

  type content_format = [
    | `Text of [ `Plain ]
    | `Application of [
        | `Link_format
        | `Xml
        | `Octet_stream
        | `Exi
        | `Json
        | `Cbor
      ]
  ]

  type option =
    | If_match of string
    | Uri_host of string
    | Etag of string
    | If_none_match
    | Observe of [ `Register | `Deregister | `Sequnce of int ]
    | Uri_port of int
    | Location_path of string
    | Uri_path of string
    | Content_format of content_format
    | Max_age of int
    | Uri_query of string
    | Accept of int
    | Location_query of string
    | Proxy_uri of string
    | Proxy_scheme of string
    | Size1 of int

  val make
     : ?version:int
    -> ?id:int
    -> ?token:string
    -> code:code
    -> ?kind:kind
    -> ?options:option list
    -> buffer
    -> t
(** Coap message constructor. *)

  val version : t -> int

  val id : t -> int

  val kind : t -> kind

  val code : t -> code

  val token : t -> string
  (** Message token used to match a response with a request.

      Token values may be 0 to 8 bytes in length. *)

  val options : t -> option list

  val path : t -> string list
  (** Extract request path from message options. *)

  val payload : t -> buffer

  val payload_length : t -> int

  val buffer_to_string : buffer -> string

  val buffer_of_string : string -> buffer

  val is_confirmable : t -> bool

  val decode : buffer -> (t, error) result

  val encode : t -> buffer

  val pp : Format.formatter -> t -> unit


  val max_size : int
  (** Maximum safe message size.

      https://tools.ietf.org/html/rfc7252#section-4.6 *)
end


module Request : sig

end

module Response : sig
  val not_found
     : ?version:int
    -> ?id:int
    -> ?token:string
    -> ?kind:Message.kind
    -> ?options:Message.option list
    -> Message.buffer
    -> Message.t

  val content
     : ?version:int
    -> ?id:int
    -> ?token:string
    -> ?kind:Message.kind
    -> ?options:Message.option list
    -> Message.buffer
    -> Message.t
end

