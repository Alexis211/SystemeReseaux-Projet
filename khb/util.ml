
module Int = struct
    type t = int
    let compare = Pervasives.compare
end

module Smap = Map.Make(String)
module Imap = Map.Make(Int)

let (^^) a b = (a || b) && (not (a && b))
