
type error = Coap_core.error
let pp_error = Coap_core.pp_error

module Request = Coap_core.Request
module Response = Coap_core.Response
module Message = Coap_core.Message

module Server = Coap_server_unix

