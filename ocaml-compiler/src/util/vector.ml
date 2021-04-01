type 'a t = {
  mutable arr : 'a Array.t;
  mutable size : int;
  mutable cap : int;
}

let empty () = { arr = [||]; size = 0; cap = 0; }

let extend_cap vec init =
  if vec.cap = 0 then begin
    vec.cap <- 1;
    vec.arr <- Array.make 2 init
  end else begin
    let arr = Array.make (2 * vec.cap) init in
    begin
      Array.blit vec.arr 0 arr 0 vec.cap;
      vec.arr <- arr;
      vec.cap <- 2 * vec.cap
    end
  end

let set vec idx ele = 
  if vec.size <= idx then
    raise (Invalid_argument ("set : idx = " ^ (string_of_int idx)))
  else 
    Array.set vec.arr idx ele

let length vec = vec.size

let push_back vec ele =
  if vec.size < vec.cap then begin
    let idx = vec.size in
    begin
      vec.size <- vec.size + 1;
      set vec idx ele
    end
  end else begin
    extend_cap vec ele;
    vec.size <- vec.size + 1
  end

let get vec idx =
  if idx < 0 || vec.size <= idx then begin
    raise (Invalid_argument ("get : idx = " ^ (string_of_int idx)))
  end else begin
    Array.get vec.arr idx
  end

let rget vec idx = 
  let idx = vec.size - (idx + 1) in
  get vec idx

let make s init =
  let arr = Array.make (2 * s) init in
  { arr = arr; size = s; cap = 2 * s; }

let pop_back vec =
  if vec.size <= 0 then
    raise (Invalid_argument "pop_back")
  else
    vec.size <- vec.size - 1

let pops vec len =
  if vec.size < len then begin
    raise (Invalid_argument ("pops : size = " ^ (string_of_int vec.size) ^ " len = " ^ (string_of_int len)))
  end else begin
    vec.size <- vec.size - len
  end

let to_list vec =
  let rec aux v l idx =
    if idx = length v then 
      l 
    else
      let hd = rget v idx in
      aux v (hd :: l) (idx + 1)
  in
  aux vec [] 0
