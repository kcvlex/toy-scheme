type 'a t = {
  mutable index : int;
  generator : int -> 'a;
}

let make gen = { index = 0; generator = gen; }

let fresh tv =
  let id = tv.index in
  tv.index <- id + 1; tv.generator id

let rec fresh_list tv n =
  if n = 0 then
    []
  else
    let x = fresh tv in
    let xs = fresh_list tv (n - 1) in
    x :: xs
