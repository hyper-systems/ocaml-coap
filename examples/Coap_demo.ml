
let (let*) res f =
  match res with
  | Ok x -> f x
  | Error e -> Error e

let return x = Ok x

let run x =
  match x with
  | Ok () -> ()
  | Error err ->
    Fmt.epr "[ERROR] %a@." Coap.pp_error err;
    exit 1


let msg1_data =
  "\x64\x45\x13\xFD\xD0\xE2\x4D\xAC\xFF\x48\x65\x6C\x6C\x6F"

let msg2_data =
  "\x44\x01\x84\x9e\x51\x55\x77\xe8\xb2\x48\x69\x04\x54\x65\x73\x74\x43\x61\x3d\x31"

let msg4_data = "\x44\x02\xd9\xb9\x74\x6f\x6b\x31\x31\x48\x43\xa9\x8a\xc7\x71\x41\xff\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67"


let recode expected =
  let* msg = Coap.Message.decode expected in
  Format.printf "recode: msg=%a@." Coap.Message.pp msg;
  let actual = Coap.Message.encode msg in
  if not (String.equal expected actual) then begin
    let expected = Cstruct.of_string expected in
    let actual = Cstruct.of_string actual in
    Format.printf "recode failed@. - %a@, + %a@]@."
      Cstruct.hexdump_pp expected Cstruct.hexdump_pp actual;
    exit 1
  end else Ok ()


let main =
  let* () = recode msg1_data in
  let* () = recode msg2_data in
  let* () = recode msg4_data in

  Coap.Server.start ()


let () = run main


