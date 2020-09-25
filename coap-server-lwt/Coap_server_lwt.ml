

let exn_udp_not_found =
  Failure "Could not find the `udp` protocol entry. Check your /etc/protocols file."


let pp_inet_addr formatter addr =
  Format.pp_print_string formatter (Unix.string_of_inet_addr addr)


let (let*) m f = Lwt.bind m f


let log ?(channel=stdout) fmt =
  let f = Format.formatter_of_out_channel channel in
  Format.(kfprintf (fun f -> pp_print_newline f ()) f fmt)


let max_coap_message_size = 1152


let start ?(host="127.0.0.1") ?(port=5683) handler =
  let proto =
    try Unix.getprotobyname "udp"
    with Not_found -> raise exn_udp_not_found in

  let inet_addr, sockaddr =
    match Unix.gethostbyname host with
    | {h_addr_list; _} ->
      let inet_addr = Array.get h_addr_list 0 in
      let sockaddr = Unix.ADDR_INET (inet_addr, port) in
      (inet_addr, sockaddr)
    | exception Not_found ->
      let inet_addr = Unix.inet_addr_of_string host in
      let sockaddr = Unix.ADDR_INET (inet_addr, port) in
      (inet_addr, sockaddr) in

  let socket =
    let domain = Unix.domain_of_sockaddr sockaddr in
    Lwt_unix.of_unix_file_descr
      (Unix.socket domain Unix.SOCK_DGRAM proto.Unix.p_proto) in

  let* () = Lwt_unix.bind socket sockaddr in

  log "[INFO] Coap.Server: Listening... addr=%a port=%d" pp_inet_addr inet_addr port;
  let req_cstruct = Cstruct.create max_coap_message_size in
  let rec loop () =
    let* incoming = Lwt_cstruct.recvfrom socket req_cstruct [] in
    match incoming with
    | len, (Unix.ADDR_INET (_client_addr, _port) as client_sockaddr) ->
      let req_buffer = Cstruct.to_bigarray (Cstruct.sub req_cstruct 0 len) in
      let req_result = Coap_core.Message.decode req_buffer in
      let* res = handler req_result in
      let res_buffer = Coap_core.Message.encode res in
      let res_cstruct = Cstruct.of_bigarray res_buffer in
      let res_len = Cstruct.len res_cstruct in
      let* res_len_sent =
        Lwt_cstruct.sendto socket res_cstruct [] client_sockaddr in
      if res_len_sent <> res_len then
        log "[ERROR] Could not send response message@.";
      loop ()
    | _ -> assert false
  in
  loop ()


