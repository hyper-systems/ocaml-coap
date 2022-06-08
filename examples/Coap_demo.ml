
let (let*) res f =
  match res with
  | Ok x -> f x
  | Error e -> Error e

let return x = Ok x

let run x =
  match x with
  | Ok () -> ()
  | Error err ->
    Fmt.epr "[ERROR] %a@." Coap_core.pp_error err;
    exit 1


let msg1_data =
  "\x64\x45\x13\xFD\xD0\xE2\x4D\xAC\xFF\x48\x65\x6C\x6C\x6F"

let msg2_data =
  "\x44\x01\x84\x9e\x51\x55\x77\xe8\xb2\x48\x69\x04\x54\x65\x73\x74\x43\x61\x3d\x31"

let msg4_data = "\x44\x02\xd9\xb9\x74\x6f\x6b\x31\x31\x48\x43\xa9\x8a\xc7\x71\x41\xff\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67"


let recode expected_str =
  let expected = Coap_core.Message.buffer_of_string expected_str in
  let* msg = Coap_core.Message.decode expected in
  Format.printf "recode: msg=%a@." Coap_core.Message.pp msg;
  let actual = Coap_core.Message.encode msg in
  let actual_str = Coap_core.Message.buffer_to_string actual in
  if not (String.equal expected_str actual_str) then begin
    let expected_cstr = Cstruct.of_bigarray expected in
    let actual_cstr = Cstruct.of_bigarray actual in
    Format.printf "recode failed@. - %a@, + %a@]@."
      Cstruct.hexdump_pp expected_cstr Cstruct.hexdump_pp actual_cstr;
    exit 1
  end else Ok ()


let test_message_encoder () = run begin
  let* () = recode msg1_data in
  let* () = recode msg2_data in
  let* () = recode msg4_data in
  Ok ()
end

let () =
  Coap_server_lwt.start begin function
    | Ok req ->
      Format.eprintf "--- REQUEST MESSAGE ---@.%a@.@." Coap_core.Message.pp req;
      let token = Coap_core.Message.token req in
      let payload = Coap_core.Message.buffer_of_string "Hello, world!" in
      let message = Coap_core.Message.make ~code:(Response `Content) ~token payload in
      Lwt.return message
    | Error e ->
      Format.eprintf "[ERROR] %a@." Coap_core.pp_error e;
      let payload = Coap_core.Message.buffer_of_string "Oh no!" in
      let message = Coap_core.Message.make ~code:(Response `Internal_server_error) payload in
      Lwt.return message
  end
  |> Lwt_main.run


