type t = int array * int array

let create n = 
  (Array.init n (fun i -> i), Array.make n 0)

let rec find s i =
  let (f, r) = s in
  if f.(i) = i then
    i
  else
    find s f.(i)

let union s i j =
  let (f, r) = s in
  let ci = find s i in
  let cj = find s j in
  if ci <> cj then
    if r.(ci) < r.(cj) then
      let () = f.(ci) <- cj in 
      if r.(ci) = r.(cj) then 
        r.(cj) <- r.(cj) + 1
    else
      let () = f.(cj) <- ci in
      if r.(ci) = r.(cj) then
        r.(ci) <- r.(ci) + 1
