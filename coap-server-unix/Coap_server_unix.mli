
val start
    : ?host:string
  -> ?port:int
  -> ((Coap_core.Message.t, Coap_core.error) result -> Coap_core.Message.t)
  -> unit

