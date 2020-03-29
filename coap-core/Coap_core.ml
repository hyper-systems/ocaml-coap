
(* Coap errors *)

type error = [
  | `Invalid_token_length
  | `Invalid_option_delta
  | `Invalid_option_length
]

let pp_error formatter x =
  Format.fprintf formatter @@
  match x with
  | `Invalid_token_length -> "@[Invalid@ token@ length@]"
  | `Invalid_option_delta -> "@[Invalid@ option@ delta@]"
  | `Invalid_option_length -> "@[Invalid@ option@ length@]"


module Request = Coap_request
module Response = Coap_response
module Message = Coap_message

