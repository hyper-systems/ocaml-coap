

let not_found
    ?version ?id ?token ?(kind=Coap_message.Acknowledgement) ?options payload =
  Coap_message.make ?version ?id ?token
    ~code:(Coap_message.Response `Not_found)
    ~kind ?options payload

let content
    ?version ?id ?token ?(kind=Coap_message.Acknowledgement) ?options payload =
  Coap_message.make ?version ?id ?token
    ~code:(Coap_message.Response `Content)
    ~kind ?options payload

let bad_request
    ?version ?id ?token ?(kind=Coap_message.Acknowledgement) ?options payload =
  Coap_message.make ?version ?id ?token
    ~code:(Coap_message.Response `Bad_request)
    ~kind ?options payload
