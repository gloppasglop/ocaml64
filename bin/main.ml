open Base
open Stdio
open Ocaml64.C64

(* let mem = Array.create ~len:65536 0xFF *)

(* let pgm = [ 0xEA; 0xA9; 0x09; 0xA9; 0x00; 0xA9; 0x89; 0xA2; 0x09; 0xA2; 0x00; 0xA2; 0x89 ] *)
(* let pgm = [ 0xEA ] *)

let () = printf "\n"
let () = printf "Hi!\n"

(* let _ = C64.create 3 *)
let load_pgm mem offset pgm =
  List.iteri pgm ~f:(fun i data ->
    printf "Loading 0x%02X to 0x%04X \n" data (offset + i);
    mem.(offset + i) <- data)
;;

let rec execute_cycles i acc (computer : Ocaml64.C64.M.t) =
  if i = 0
  then acc |> List.rev
  else (
    let computer' = M.fetch_decode_execute computer in
    execute_cycles (i - 1) (computer :: acc) computer')
;;

let _init_test_computer mem pgm =
  load_pgm mem 0x1000 pgm;
  let computer = M.create mem in
  { computer with cpu = { computer.cpu with pc = 0x1000; address = 0x1000 } }
;;

open Ocaml64.C6510.M

let init_test_computer =
  let mem = Array.create ~len:65536 0xFF in
  let address = 0x0000 in
  let data = 0xFF in
  let bus = M.{ data; address } in
  let computer = M.create mem in
  { computer with cpu = { computer.cpu with pc = address; address; data }; bus }
;;

let dump_execution (computer : M.t) =
  printf
    "ab: 0x%04X db: 0x%02X %s\n"
    computer.bus.address
    computer.bus.data
    (Ocaml64.C6510.M.cpu_to_string computer.cpu)
;;

let dump_executions = List.iter ~f:dump_execution

(* let dump_last_execution executions = List.hd_exn executions |> dump_execution *)
let cycles = 12 * 2

(*
  # 1000
  LDA #10
  JMP 0xC000

  # 0xC000
  LDA #20

*)
let computer = init_test_computer
let start_address = 0x1000
let mem = computer.banks

(* let pgm1 = [ 0xA9; 0x10; 0x4C; 0x00; 0xC0 ] *)
let pgm1 = [ 0xA9; 0x01; 0xA2; 0xFF; 0xA0; 0x03; 0xB5; 0x44; 0xEA ]
let () = load_pgm mem start_address pgm1
let pgm2 = [ 0xA9; 0x20 ]
let () = load_pgm mem 0xC000 pgm2
let data = List.hd_exn pgm1
let () = mem.(0x4F) <- 0x04
let () = mem.(0x44) <- 0x50

let computer =
  { computer with
    cpu = { computer.cpu with pc = start_address; address = start_address }
  ; bus = { data; address = start_address }
  }
;;

(* let computer = init_test_computer mem pgm *)

let () =
  printf "%s\n" (cpu_to_string computer.cpu);
  let executions = execute_cycles cycles [] computer in
  dump_executions executions
;;
