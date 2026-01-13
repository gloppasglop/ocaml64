open Base
open Stdio
open Ocaml64.C6502.Cpu

let () =
  printf "";
  let opcodes =
    [ 0xEA; 0x2A; 0xA9; 0xA5; 0xB5; 0xAD; 0xBD; 0xB9; 0xA1; 0xB1; 0xB6; 0x6C; 0x90 ]
    (* NOP*)
  in
  List.iter opcodes ~f:(fun opcode ->
    let { inst; mode; cycles; bytes } = decode opcode in
    printf "%s : Cycles=%d, Bytes = %d\n" (inst_to_string inst mode) cycles bytes)
;;

let cpu =
  { (* Pins *)
    rdy = true (* Ready *)
  ; irq = false (* IRQ - Inverted *)
  ; nmi = false (* Non Maskable Interrrupt - Inverted *)
  ; phy1 = true (* phy1 , IN*)
  ; phy2 = true (* phy2 , OUT*)
  ; aec = true (* Address Enable Control *)
  ; rw = true (* R/W Read/Write *)
  ; reset = false (* Reset - Inverted *)
  ; address = 0x1000 (* Pins A0 to A15 *)
  ; data = 0xEA (* Pins DB0 to DB7 *)
  ; ioport = 0 (* Pins P0 to  P7 *)
  ; a = 0 (* A register *)
  ; x = 0 (* X register *)
  ; y = 0 (* Y register *)
  ; sr = 0 (* Status register *)
  ; pc = 0x1000 (* Program counter *)
  ; sp = 0xFF (* Stack pointer *)
  ; ir =
      decode 0x00
      (* Internal instruction register TODO: Should it be directly teh decoded instruction *)
  ; cycle = 1
  }
;;

let () =
  let s = sexp_of_cpu cpu in
  let o = Sexplib.Sexp.to_string_hum s in
  printf "%s\n" o
;;

open Ocaml64.C6502

let mem = Array.create ~len:65536 0xFF

(* let pgm = [ 0xEA; 0xA9; 0x09; 0xA9; 0x00; 0xA9; 0x89; 0xA2; 0x09; 0xA2; 0x00; 0xA2; 0x89 ] *)
let pgm = [ 0xEA ]
let load_pgm mem offset pgm = List.iteri pgm ~f:(fun i data -> mem.(offset + i) <- data)
let () = load_pgm mem 0x1000 pgm
let computer = Computer.create mem
let computer = { computer with cpu = { computer.cpu with pc = 0x1000; address = 0x1000 } }

(* let () = mem.(0x1000) <- 0xEA *)
(* let () = mem.(0x1001) <- 0xA9 *)
(* let () = mem.(0x1002) <- 0xFF *)
let () = printf "%s\n" (Ocaml64.C6502.Cpu.cpu_to_string cpu)

let rec loop computer =
  let computer = Computer.fetch_decode_execute computer in
  printf "%s\n" (Cpu.cpu_to_string computer.cpu);
  loop computer
;;

let () = loop computer
