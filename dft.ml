open Fft_domain

module IntegerModulo(Param: sig val q: int val omega: int end) = struct
  type t = Int.t
  let q = Param.q
  let omega = Param.omega
  let add x y = (Int.add x y) mod q
  let mul x y = (Int.mul x y) mod q
  let sub x y = (Int.sub (Int.add (q * 3) x) y) mod q
  let zero = 0
  let one = 1

  let primitive_root_power _ j =
    let rec power i acc =
      if i = 0 then acc
      else
        let update = mul acc omega in
        power (i - 1) update
    in
    power j 1

end

module DFT(Param: sig val q: int val omega: int end) = struct
  module D = IntegerModulo(Param)
  module Prim_roots = Primitive_roots(D)
  let dft input =
    let n = Array.length input in
    let dot xs ys = Array.fold_left (fun acc (x, y) -> D.add acc (D.mul x y))
        D.zero (Array.map2 (fun x y -> (x, y)) xs ys) in
    Array.init n (fun i ->
        let coeffs = Prim_roots.powers n n i in
        dot input coeffs)
end
