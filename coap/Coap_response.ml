
type kind = [
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

let dump_kind f k =
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

