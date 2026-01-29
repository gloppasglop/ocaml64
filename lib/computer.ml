module type S = sig
  type t
  type memory_banks

  val fetch_decode_execute : t -> t
  val create : memory_banks -> t
  (* val computer_to_string : t -> string *)
end
