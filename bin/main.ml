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
    printf "Loading 0x%02X to 0x%04X " data (offset + i);
    mem.(offset + i) <- data)
;;

let rec execute_cycles i acc (computer : Ocaml64.C64.M.t) =
  if i = 0
  then acc |> List.rev
  else (
    let computer' = M.fetch_decode_execute computer in
    execute_cycles (i - 1) (computer :: acc) computer')
;;

let init_test_computer mem pgm =
  load_pgm mem 0x1000 pgm;
  let computer = M.create mem in
  { computer with cpu = { computer.cpu with pc = 0x1000; address = 0x1000 } }
;;

open Ocaml64.C6510.M

let dump_executions =
  let open Ocaml64.C6510.M in
  List.iter ~f:(fun (computer : M.t) -> cpu_to_string computer.cpu |> printf "%s")
;;

let cycles = 3
let pgm = [ 0x65; 0x44 ]
let mem = Array.create ~len:65536 0xFF
let () = load_pgm mem 0x1000 pgm
let computer = init_test_computer mem pgm

let () =
  printf "%s\n" (cpu_to_string computer.cpu);
  let executions = execute_cycles cycles [] computer in
  dump_executions executions
;;
