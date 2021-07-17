open Tagless_impl
open Tagless_fft

module NewHope_param = struct
  let q = 12289

  let qinv = 12287

  let omega = 49

  let n = 1024

end

module R_Int_mod = IntegerModulo_lazy(R_modulo)(NewHope_param)

let test_ntt_R =
  let module R_NTT = FFT_lazy_gen(R_Array)(R_Int_mod) in
  let open R_Array in
  let open Dft in
  let open DFT(Fft_types.IntegerModulo(NewHope_param)) in
  let size = 1024 in
  let arr_ref = Array.init size (fun _ -> Random.int NewHope_param.q) in
  let ref_res = dft arr_ref in
  let arr = (arr_init size (fun i -> int_ (arr_ref.(i)))) () in
  let fn = (R_NTT.fft size) () in
  fn arr;
  ignore(Array.init size (fun i ->
      let res = arr.(i) mod NewHope_param.q in
      Printf.printf "ref %d, tagless %d\n" ref_res.(i) res;
      assert(ref_res.(i) = res)))
