
open Coap_base


let max_coap_message_size = 1152

let sock =
  Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
    (Unix.getprotobyname "udp").Unix.p_proto


let start ?(host="127.0.0.1") ?(port=5683) handler =
  let addr = Unix.inet_addr_of_string host in
  Unix.bind sock (Unix.ADDR_INET (addr, port));
  info "Coap.Server: Listening... host=%S port=%d" host port;
  let buffer = Bytes.create max_coap_message_size in
  let rec loop () =
    match Unix.recvfrom sock buffer 0 max_coap_message_size [] with
    | len, (Unix.ADDR_INET (client_addr, _port) as client_sockaddr) ->
      let client_host = (Unix.gethostbyaddr client_addr).Unix.h_name in
      let req = Bytes.to_string (Bytes.sub buffer 0 len) in
      let req_result = Coap_message.decode req in

      let () = ignore (`Add_this_to_req client_host) in

      let res = handler req_result in
      let res = Bytes.of_string (Coap_message.encode (res)) in
      let res_len = Bytes.length res in
      let res_len_sent =
        Unix.sendto sock res 0 res_len [] client_sockaddr in
      if res_len_sent <> res_len then
        warning "Could not send response message@.";
      loop ()
    | _ -> assert false
  in
  loop ()


