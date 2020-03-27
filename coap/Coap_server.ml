
open Local

let max_coap_message_size = 1152
let port = 5683

let sock =
  Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
    (Unix.getprotobyname "udp").Unix.p_proto

let start () =
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  print "[INFO] Coap.Server: Listening on port %d" port;
  let buffer = Bytes.create max_coap_message_size in
  let rec loop () =
    match Unix.recvfrom sock buffer 0 max_coap_message_size [] with
    | len, (Unix.ADDR_INET (addr, _port) as client_addr) ->
      let client_host = (Unix.gethostbyaddr addr).Unix.h_name in
      let req = Bytes.to_string (Bytes.sub buffer 0 len) in
      Cstruct.of_string req |> Cstruct.hexdump;
      let* req = Coap_message.decode req in
      print "(client %S) (req %a)" client_host Coap_message.pp req;

      let res = Coap_message.make
          ~code:(Response `Content)
          ~token:(Coap_message.token req)
          ~kind:(if Coap_message.is_confirmable req
                 then Acknowledgement else Confirmable)
          "Hey" in
      let res = Bytes.of_string (Coap_message.encode res) in
      let res_len = Bytes.length res in
      let _ = Unix.sendto sock res 0 res_len [] client_addr in
      loop ()
    | _ -> assert false in
  loop ()


