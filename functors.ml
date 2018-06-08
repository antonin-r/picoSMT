module Sat_assoc = Map.Make(struct type t = int let compare = compare end)
