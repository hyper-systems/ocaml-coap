
let () = Printexc.record_backtrace true

let byte = Char.chr

let (let*) res f =
  match res with
  | Ok x -> f x
  | Error e -> Error e

let return x = Ok x

type 'a printer = Format.formatter -> 'a -> unit

let inspect (pp : 'a printer) =
  Format.(kfprintf (fun f -> pp_print_newline f ()) std_formatter "%a" pp)

let print ?(channel=stdout) fmt =
  let f = Format.formatter_of_out_channel channel in
  Format.(kfprintf (fun f -> pp_print_newline f ()) f fmt)

let debug fmt = print ~channel:stderr ("[DEBUG] " ^^ fmt)
let error fmt = print ~channel:stderr ("[ERROR] " ^^ fmt)
let info  fmt = print ~channel:stderr ("[INFO] " ^^ fmt)

let int_to_bin_byte d =
  if d < 0 then invalid_arg "bin_of_int" else
  if d = 0 then "0" else
  let rec aux acc d =
    if d = 0 then acc else
    aux (string_of_int (d land 1) :: acc) (d lsr 1)
  in
  let res = String.concat "" (aux [] d) in
  if String.length res mod 8 <> 0 then
     String.make (8 - String.length res mod 8) '0' ^ res
  else res

let print_cstruct_bin cstruct =
  for i = 0 to Cstruct.len cstruct - 1 do
    let byte = Cstruct.get_uint8 cstruct i in
    print_endline (int_to_bin_byte byte)
  done


let gen_id =
  let n = ref 0 in fun () -> begin
    incr n;
    if !n >= 0xFFFF then n := 0;
    !n
  end

