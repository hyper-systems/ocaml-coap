
let (let*) m f = Lwt.bind m f


let log ?(channel=stdout) fmt =
  let f = Format.formatter_of_out_channel channel in
  Format.(kfprintf (fun f -> pp_print_newline f ()) f fmt)


let max_coap_message_size = 1152

let sock =
  let proto = try Unix.getprotobyname "udp" with Not_found ->
    failwith "Could not find the `udp` protocol entry. Check your /etc/protocols file." in
  Lwt_unix.of_unix_file_descr
    (Unix.socket Unix.PF_INET Unix.SOCK_DGRAM
      proto.Unix.p_proto)


let start ?(host="127.0.0.1") ?(port=5683) handler =
  let addr = Unix.inet_addr_of_string host in
  let* () = Lwt_unix.bind sock (Unix.ADDR_INET (addr, port)) in

  log "[INFO] Coap.Server: Listening... host=%S port=%d" host port;
  let buffer = Bytes.create max_coap_message_size in
  let rec loop () =
    let* incoming = Lwt_unix.recvfrom sock buffer 0 max_coap_message_size [] in
    match incoming with
    | len, (Unix.ADDR_INET (client_addr, _port) as client_sockaddr) ->
      let client_host = (Unix.gethostbyaddr client_addr).Unix.h_name in
      let req = Bytes.to_string (Bytes.sub buffer 0 len) in
      let req_result = Coap_core.Message.decode req in

      let () = ignore (`Add_this_to_req client_host) in

      let* res = handler req_result in
      let res = Bytes.of_string (Coap_core.Message.encode (res)) in
      let res_len = Bytes.length res in
      let* res_len_sent =
        Lwt_unix.sendto sock res 0 res_len [] client_sockaddr in
      if res_len_sent <> res_len then
        log "[ERROR] Could not send response message@.";
      loop ()
    | _ -> assert false
  in
  loop ()


