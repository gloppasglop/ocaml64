module type S = sig
  type instruction
  type addressingmode
  type cpu

  val instruction_to_string : instruction -> string
  val inst_to_string : instruction -> addressingmode -> string
  val cpu_to_string : cpu -> string
  val tick : cpu -> cpu
end
