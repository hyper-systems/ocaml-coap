

let (let*) res f =
  match res with
  | Ok x -> f x
  | Error e -> Error e

let return x = Ok x


(* Message kind *)

type kind =
  | Confirmable
  | Nonconfirmable
  | Acknowledgement
  | Reset

let pp_kind f k =
  let p = Format.fprintf f in
  match k with
  | Confirmable -> p "Confirmable"
  | Nonconfirmable -> p "Nonconfirmable"
  | Acknowledgement -> p "Acknowledgement"
  | Reset -> p "Reset"

let kind_of_int n =
  match n with
  | 0 -> Confirmable
  | 1 -> Nonconfirmable
  | 2 -> Acknowledgement
  | 3 -> Reset
  | _ -> invalid_arg (string_of_int n ^ "is not a valid message kind")

let kind_to_int k =
  match k with
  | Confirmable -> 0
  | Nonconfirmable -> 1
  | Acknowledgement -> 2
  | Reset -> 3


(* Message code *)

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


let dump_request_kind f k =
  let p = Format.fprintf f in
  match k with
  | `Get -> p "`Get"
  | `Post -> p "`Post"
  | `Put -> p "`Put"
  | `Delete -> p "`Delete"


let dump_response_kind f k =
  let p = Format.fprintf f in
  match k with
  | `Created -> p "`Created"
  | `Deleted -> p "`Deleted"
  | `Valid -> p "`Valid"
  | `Changed -> p "`Changed"
  | `Content -> p "`Content"
  | `Continue -> p "`Continue"
  | `Bad_request -> p "`Bad_request"
  | `Unauthorized -> p "`Unauthorized"
  | `Bad_option -> p "`Bad_option"
  | `Forbidden -> p "`Forbidden"
  | `Not_found -> p "`Not_found"
  | `Method_not_allowed -> p "`Method_not_allowed"
  | `Not_acceptable -> p "`Not_acceptable"
  | `Precondition_failed -> p "`Precondition_failed"
  | `Request_entity_too_large -> p "`Request_entity_too_large"
  | `Unsupported_content_format -> p "`Unsupported_content_format"
  | `Request_entity_incomplete -> p "`Request_entity_incomplete"
  | `Too_many_requests -> p "`Too_many_requests"
  | `Internal_server_error -> p "`Internal_server_error"
  | `Not_implemented -> p "`Not_implemented"
  | `Bad_gateway -> p "`Bad_gateway"
  | `Service_unavailable -> p "`Service_unavailable"
  | `Gateway_timeout -> p "`Gateway_timeout"
  | `Proxying_not_supported -> p "`Proxying_not_supported"


let pp_code f x =
  match x with
  | Empty -> Format.fprintf f "Empty"
  | Request x -> Format.fprintf f "Request %a" dump_request_kind x
  | Response x -> Format.fprintf f "Response %a" dump_response_kind x


(* Message content format *)

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


(* Message header *)

module Header = struct
  type t = int32

  let make ~version ~id ~token_length ~kind ~code =
    let self = Int32.shift_left (Int32.of_int version) 30 in
    let self = Int32.add self (Int32.of_int (kind_to_int kind lsl 28)) in
    let self = Int32.add self (Int32.of_int (token_length lsl 24)) in
    let encode_code code data =
      let self = Int32.add self (Int32.of_int (code lsl 21)) in
      let self = Int32.add self (Int32.of_int (data lsl 16)) in
      self in
    let self =
      match code with
      | Empty -> encode_code 0 00
      | Request `Get -> encode_code 0 01
      | Request `Post -> encode_code 0 02
      | Request `Put -> encode_code 0 03
      | Request `Delete -> encode_code 0 04
      | Response `Created -> encode_code 2 01
      | Response `Deleted -> encode_code 2 02
      | Response `Valid -> encode_code 2 03
      | Response `Changed -> encode_code 2 04
      | Response `Content -> encode_code 2 05
      | Response `Bad_request -> encode_code 4 00
      | Response `Unauthorized -> encode_code 4 01
      | Response `Bad_option -> encode_code 4 02
      | Response `Forbidden -> encode_code 4 03
      | Response `Not_found -> encode_code 4 04
      | Response `Method_not_allowed -> encode_code 4 05
      | Response `Not_acceptable -> encode_code 4 06
      | Response `Precondition_failed -> encode_code 4 12
      | Response `Request_entity_too_large -> encode_code 4 13
      | Response `Unsupported_content_format -> encode_code 4 15
      | Response `Internal_server_error -> encode_code 5 00
      | Response `Not_implemented -> encode_code 5 01
      | Response `Bad_gateway -> encode_code 5 02
      | Response `Service_unavailable -> encode_code 5 03
      | Response `Gateway_timeout -> encode_code 5 04
      | Response `Proxying_not_supported -> encode_code 5 05 in
    let self = Int32.add self (Int32.of_int id) in
    self

  let version self =
    Int32.shift_right_logical self 30
    |> Int32.to_int

  let kind self =
    let mask, shift = 0b00110000000000000000000000000000, 28 in
    Int32.(shift_right_logical (logand self (of_int mask))) shift
    |> Int32.to_int
    |> kind_of_int

  let token_length self =
    let mask, shift = 0b00001111000000000000000000000000, 24 in
    Int32.(shift_right_logical (logand self (of_int mask))) shift
    |> Int32.to_int

  let code self =
    let mask, shift = 0b00000000111000000000000000000000, 21 in
    let mask = Int32.of_int mask in
    let code = Int32.(shift_right_logical (logand self mask)) shift in
    let code = Int32.to_int code in

    let mask, shift = 0b00000000000111110000000000000000, 16 in
    let mask = Int32.of_int mask in
    let data = Int32.(shift_right_logical (logand self mask)) shift in
    let data = Int32.to_int data in
    match code, data with
    | 0, 00 -> Empty
    | 0, 01 -> Request `Get
    | 0, 02 -> Request `Post
    | 0, 03 -> Request `Put
    | 0, 04 -> Request `Delete

    | (1|3), _ -> failwith "Reserved"

    | 2, 01 -> Response `Created
    | 2, 02 -> Response `Deleted
    | 2, 03 -> Response `Valid
    | 2, 04 -> Response `Changed
    | 2, 05 -> Response `Content

    | 4, 00 -> Response `Bad_request
    | 4, 01 -> Response `Unauthorized
    | 4, 02 -> Response `Bad_option
    | 4, 03 -> Response `Forbidden
    | 4, 04 -> Response `Not_found
    | 4, 05 -> Response `Method_not_allowed
    | 4, 06 -> Response `Not_acceptable
    | 4, 12 -> Response `Precondition_failed
    | 4, 13 -> Response `Request_entity_too_large
    | 4, 15 -> Response `Unsupported_content_format

    | 5, 00 -> Response `Internal_server_error
    | 5, 01 -> Response `Not_implemented
    | 5, 02 -> Response `Bad_gateway
    | 5, 03 -> Response `Service_unavailable
    | 5, 04 -> Response `Gateway_timeout
    | 5, 05 -> Response `Proxying_not_supported

    | _ -> failwith "Unassigned message code"


  let id self =
    let mask = 0b00000000000000001111111111111111 in
    Int32.(logand self (of_int mask))
    |> Int32.to_int

  let pp f self =
    Format.fprintf f
      "(@[<2>Coap.Header@ (version %d)@ (kind %a)@ (token_length %d)@ \
      (code %a)@ (id %d)@])"
      (version self)
      pp_kind (kind self)
      (token_length self)
      pp_code (code self)
      (id self)
end


(* Message options *)

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

(* https://tools.ietf.org/html/rfc7252#section-5.10.1 *)
module Option = struct
  type t = option

  let content_format_of_int n =
    match n with
    | 0  -> `Text `Plain
    | 40 -> `Application `Link_format
    | 41 -> `Application `Xml
    | 42 -> `Application `Octet_stream
    | 47 -> `Application `Exi
    | 50 -> `Application `Json
    (* https://tools.ietf.org/html/rfc7049#section-7.4 *)
    | 60 -> `Application `Cbor
    | _ -> invalid_arg ("Unsupported content format" ^ string_of_int n)

  let content_format_to_int n =
    match n with
    | `Text `Plain -> 0
    | `Application `Link_format -> 40
    | `Application `Xml -> 41
    | `Application `Octet_stream -> 42
    | `Application `Exi -> 47
    | `Application `Json -> 50
    (* https://tools.ietf.org/html/rfc7049#section-7.4 *)
    | `Application `Cbor -> 60

  let decode_int value length =
    (* debug "(decode_int (value:str %S) (value:hex %a) (length %d))" *)
    (*   (Cstruct.to_string value) *)
    (*   Cstruct.hexdump_pp value length; *)
    if length = 0 then 0 else
    if length = 1 then
      Cstruct.get_uint8 value 0
    else
    if length = 2 then
      Cstruct.BE.get_uint16 value 0
    else
    if length = 3 then
      let a = Cstruct.get_uint8 value 0 in
      let b = Cstruct.get_uint8 value 1 in
      let c = Cstruct.get_uint8 value 2 in
      (a lsl 16) lor (b lsl 8) lor c
    else
    if length = 4 then
      Int32.to_int (Cstruct.BE.get_uint32 value 0)
    else
      invalid_arg ("option length("^ string_of_int length ^") > 4")


  let observe_of_int n =
    match n with
    | 0 -> `Register
    | 1 -> `Deregister
    | n -> `Sequnce n


  let observe_to_int n =
    match n with
    | `Register   -> 0
    | `Deregister -> 1
    | `Sequnce n  -> n


  let decode n value length =
    match n with
    | 1 -> If_match (Cstruct.to_string value)
    | 3 -> Uri_host (Cstruct.to_string value)
    | 4 -> Etag (Cstruct.to_string value)
    | 5 -> If_none_match
    | 6 -> Observe (observe_of_int (decode_int value length))
    | 7 -> Uri_port (decode_int value length)
    | 8 -> Location_path (Cstruct.to_string value)
    | 11 -> Uri_path (Cstruct.to_string value)
    | 12 -> Content_format (content_format_of_int (decode_int value length))
    | 14 -> Max_age (decode_int value length)
    | 15 -> Uri_query (Cstruct.to_string value)
    | 17 -> Accept (decode_int value length)
    | 20 -> Location_query (Cstruct.to_string value)
    | 35 -> Proxy_uri (Cstruct.to_string value)
    | 39 -> Proxy_scheme (Cstruct.to_string value)
    | 60 -> Size1 (decode_int value length)
    | 128 | 132 | 136 | 140 -> invalid_arg "Reserved option number"
    | _ -> invalid_arg "Unknown option number"


  let encode_int x =
    let data =
      if x <= 0xFF then
        (let out = Cstruct.create 1 in
         Cstruct.set_uint8 out 0 x;
         out)
      else
      if x <= 0xFFFF then
        (let out = Cstruct.create 2 in
         Cstruct.BE.set_uint16 out 0 x;
         out)
      else
      if x <= 0xFFFFFF then
        (let out = Cstruct.create 3 in
         Cstruct.BE.set_uint16 out 0 (x lsr 8);
         Cstruct.set_uint8 out 2 (x land 0xFF);
         out)
      else
        (let out = Cstruct.create 4 in
         Cstruct.BE.set_uint32 out 0 (Int32.of_int x);
         out) in
    Cstruct.to_string data


  let encode self =
    match self with
    | If_match x -> (1, x)
    | Uri_host x -> (3, x)
    | Etag x -> (4, x)
    | If_none_match -> (5, "")
    | Observe x -> (6, encode_int (observe_to_int x))
    | Uri_port x -> (7, encode_int x)
    | Location_path x -> (8, x)
    | Uri_path x -> (11, x)
    | Content_format x -> (12, encode_int (content_format_to_int x))
    | Max_age x -> (14, encode_int x)
    | Uri_query x -> (15, x)
    | Accept x -> (17, encode_int x)
    | Location_query x -> (20, x)
    | Proxy_uri x -> (35, x)
    | Proxy_scheme x -> (39, x)
    | Size1 x -> (60, encode_int x)



  let number self =
    match self with
    | If_match _ -> 1
    | Uri_host _ -> 3
    | Etag _ -> 4
    | If_none_match -> 5
    | Observe _ -> 6
    | Uri_port _ -> 7
    | Location_path _ -> 8
    | Uri_path _ -> 11
    | Content_format _ -> 12
    | Max_age _ -> 14
    | Uri_query _ -> 15
    | Accept _ -> 17
    | Location_query _ -> 20
    | Proxy_uri _ -> 35
    | Proxy_scheme _ -> 39
    | Size1 _ -> 60

  let name self =
    match self with
    | 1 -> "If_match"
    | 3 -> "Uri_host"
    | 4 -> "Etag"
    | 5 -> "If_none_mat"
    | 6 -> "Observe"
    | 7 -> "Uri_port"
    | 8 -> "Location_path"
    | 11 -> "Uri_path"
    | 12 -> "Content_format"
    | 14 -> "Max_age"
    | 15 -> "Uri_query"
    | 17 -> "Accept"
    | 20 -> "Location_query"
    | 35 -> "Proxy_uri"
    | 39 -> "Proxy_scheme"
    | 60 -> "Size1"
    | _ -> invalid_arg "Unknown option number"


  let value_length self =
    let uint_length n =
      if n <= 0xFF     then 1 else
      if n <= 0xFFFF   then 2 else
      if n <= 0xFFFFFF then 3 else
      4
    in
    match self with
    | If_none_match -> 0
    | If_match x
    | Uri_host x
    | Etag x
    | Location_path x
    | Uri_path x
    | Location_query x
    | Proxy_uri x
    | Proxy_scheme x
    | Uri_query x -> String.length x
    | Content_format _ -> 666 (* FIXME *)
    | Observe x -> uint_length (observe_to_int x)
    | Uri_port x
    | Max_age x
    | Accept x
    | Size1 x -> uint_length x


  let length self =
    let extended_delta = 0 in (* FIXME *)
    let extended_length = 0 in (* FIXME *)
    1 + extended_delta + extended_length + value_length self


  let compare a b =
    let compare : int -> int -> int = Stdlib.compare in
    compare (number a) (number b)


  let pp_observe f observe =
    match observe with
    | `Register -> Format.fprintf f "Register"
    | `Deregister -> Format.fprintf f "Deregister"
    | `Sequnce n -> Format.fprintf f "@[(Sequence %d)@]" n

  let pp f self =
    let p fmt = Format.fprintf f fmt in
    match self with
    | If_match x -> p "@[(If_match %S)@]" x
    | Uri_host x -> p "@[(Uri_host %S)@]" x
    | Etag x -> p "@[(Etag %S)@]" x
    | If_none_match -> p "If_none_match"
    | Observe x -> p "@[(Option %a)@]" pp_observe x
    | Uri_port x -> p "@[(Uri_port %d)@]" x
    | Location_path x -> p "@[(Location_path %S)@]" x
    | Uri_path x -> p "@[(Uri_path %S)@]" x
    | Content_format _ -> p "Content_format"
    | Max_age x -> p "@[(Max_age %d)@]" x
    | Uri_query x -> p "@[(Uri_query %S)@]" x
    | Accept x -> p "@[(Accept %d)@]" x
    | Location_query _ -> p "Location_query"
    | Proxy_uri x -> p "@[(Proxy_uri %S)@]" x
    | Proxy_scheme x -> p "@[(Proxy_scheme %S)@]" x
    | Size1 x -> p "@[(Size1 %d)@]" x

end


type buffer = (
  char,
  Bigarray_compat.int8_unsigned_elt,
  Bigarray_compat.c_layout
) Bigarray_compat.Array1.t


type t = {
  header : Int32.t;
  token : string;
  options : option list;
  payload : buffer;
}

let buffer_to_string payload =
  Cstruct.of_bigarray payload
  |> Cstruct.to_string


let buffer_of_string string =
  Cstruct.of_string string
  |> Cstruct.to_bigarray


let pp_options =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_space
    Option.pp

let version self =
  Header.version self.header

let kind self =
  Header.kind self.header

let code self =
  Header.code self.header

let id self =
  Header.id self.header

let token self =
  self.token

let options self =
  self.options

let payload self =
  self.payload


let payload_length self =
  Bigarray_compat.Array1.dim self.payload



let path self =
  List.fold_left
    (fun r opt -> match opt with Uri_path x -> x :: r | _ -> r)
    [] self.options
  |> List.rev

let payload_marker = 0xFF
let header_length = 4


let is_confirmable self =
  match kind self with
  | Confirmable -> true
  | _ -> false


let length self =
  let token_length = String.length self.token in
  let payload_length = Bigarray_compat.Array1.dim self.payload in
  let payload_length =
    if payload_length <> 0 then payload_length + 1
    else payload_length in
  let options_length =
    List.fold_left
      (fun r option -> r + Option.length option) 0
      self.options
  in
  header_length + token_length + options_length + payload_length


(* Message decoding *)

let decode buffer =
  let data = Cstruct.of_bigarray buffer in
  let header = Cstruct.BE.get_uint32 data 0 in

  let token_length = Header.token_length header in
  if token_length > 8 then Error `Invalid_token_length else
    let token = Cstruct.sub data header_length token_length in
    let token = Cstruct.to_string token in

    let rec decode_options i prev_delta options =
      if i >= Cstruct.len data then Ok (List.rev options, i) else
      let byte0 = Cstruct.get_uint8 data i in
      let i = i + 1 in
      if byte0 = payload_marker then Ok (List.rev options, i) else
      let* delta, i =
        match byte0 lsr 4 with
        | 13 -> Ok (Cstruct.get_uint8 data i + 13, i + 1)
        | 14 -> Ok (Cstruct.BE.get_uint16 data i + 269, i + 2)
        | 15 -> Error `Invalid_option_delta
        | other -> Ok (other, i) in
      let number = delta + prev_delta in
      let* length, i =
        match byte0 land 0xF with
        | 13 -> Ok (Cstruct.get_uint8 data i + 13, i + 1)
        | 14 -> Ok (Cstruct.BE.get_uint16 data i + 269, i + 2)
        | 15 -> Error `Invalid_option_length
        | other -> Ok (other, i) in
      let value = Cstruct.sub data i length in
      let i = i + length in
      try
        let option = Option.decode number value length in
        decode_options i number (option :: options)
      with exn ->
        Format.eprintf "[ERROR] Coap_message: exn = %s@." (Printexc.to_string exn);
        Format.eprintf "[ERROR] Coap_message: Ignoring invalid CoAP option %d...@." number;
        decode_options i number options
    in
    let* options, i = decode_options (4 + token_length) 0 [] in
    let payload = Cstruct.sub data i (Cstruct.len data - i) in
    let payload = Cstruct.to_bigarray payload in
    return { header; token; options; payload }



(* Message encoding *)

let encode_header data i header =
  Cstruct.BE.set_uint32 data i header;
  i + header_length


let encode_token data i token =
  let token_length = String.length token in
  if token_length > 0 then begin
    Cstruct.blit_from_string token 0 data i token_length;
    i + token_length
  end else i


let encode_option_part x =
  if x > 268 then 14 else
  if x > 12  then 13 else
  x


let put_option_part ~data ~i ~length value =
  if length = 13 then
    (Cstruct.set_uint8 data i (value - 13); i + 1)
  else
  if length = 14 then
    (Cstruct.BE.set_uint16 data i (value - 269); i + 2) else
  i


let encode_options data i options =
  let rec loop i prev_number options =
    match options with
    | [] -> i
    | option :: options ->
      let number, value = Option.encode option in

      let next_delta = number - prev_number in
      let length = String.length value in

      let next_delta0 = encode_option_part next_delta in
      let length0 = encode_option_part length in

      let byte0 = (next_delta0 lsl 4) lor (length0 land 0x0F) in
      Cstruct.set_uint8 data i byte0;
      let i = i + 1 in

      let i = put_option_part ~data ~i ~length:next_delta0 next_delta in
      let i = put_option_part ~data ~i ~length:length0 length in
      Cstruct.blit_from_string value 0 data i length;
      let i = i + length in

      loop i number options
  in
  loop i 0 (List.sort Option.compare options)


let encode_payload data i payload =
  let payload_length = Bigarray_compat.Array1.dim payload in
  if payload_length > 0 then begin
    Cstruct.set_char data i (Char.chr payload_marker);
    let i = i + 1 in
    let payload = Cstruct.of_bigarray payload in
    Cstruct.blit payload 0 data i payload_length;
    i + payload_length
  end else i


let encode self =
  let data = Cstruct.create (length self) in
  let i = 0 in
  let i = encode_header data i self.header in
  let i = encode_token data i self.token in
  let i = encode_options data i self.options in
  let _ = encode_payload data i self.payload in
  Cstruct.to_bigarray data



let gen_id =
  let n = ref 0 in fun () -> begin
    incr n;
    if !n >= 0xFFFF then n := 0;
    !n
  end


let make
    ?(version=1) ?(id=gen_id()) ?(token="") ~code ?(kind=Confirmable)
    ?(options=[]) payload =
  let token_length = String.length token in
  let header = Header.make ~version ~id ~token_length ~kind ~code in
  { header; token; options; payload }



let pp f self =
  Format.fprintf f
    "(@[<2>Coap.Message@ \
     (version %d)@ \
     (id %d)@ \
     (token %S)@ \
     (kind %a)@ \
     (code %a)@ \
     @[<2>(options@ %a@])@ \
     (payload %S))@]"
    (version self)
    (id self)
    (token self)
    pp_kind (kind self)
    pp_code (code self)
    pp_options (options self)
    (payload self |> buffer_to_string)

let max_size = 1152

