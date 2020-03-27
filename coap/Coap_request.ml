
type kind = [
  | `Get
  | `Post
  | `Put
  | `Delete
]

let dump_kind f k =
  let p = Format.fprintf f in
  match k with
  | `Get -> p "`Get"
  | `Post -> p "`Post"
  | `Put -> p "`Put"
  | `Delete -> p "`Delete"

