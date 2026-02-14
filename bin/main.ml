open Base
open Stdio
open Ocaml64.C64
open Mosaic

(* let load_pgm mem offset pgm = *)
(* List.iteri pgm ~f:(fun i data -> *)
(* printf "Loading 0x%02X to 0x%04X \n" data (offset + i); *)
(* mem.(offset + i) <- data) *)
(* ;; *)

(* let execute_cycles cycles computer = *)
(* let half_cycles = 2 * cycles in *)
(* let rec aux n acc computer = *)
(* if n = 0 *)
(* then Some computer :: acc *)
(* else ( *)
(* match M.fetch_decode_execute computer with *)
(* | None -> acc *)
(* | Some computer' -> aux (n - 1) (Some computer :: acc) computer') *)
(* in *)
(* aux half_cycles [] computer |> List.rev *)
(* ;; *)

(* let dump_execution n (computer : M.t option) = *)
(* match computer with *)
(* | None -> failwith "Impossible" *)
(* | Some computer -> *)
(* printf *)
(* "%10d ab: 0x%04X db: 0x%02X %s\n" *)
(* n *)
(* computer.bus.address *)
(* computer.bus.data *)
(* (Ocaml64.C6510.M.cpu_to_string computer.cpu) *)
(* ;; *)

(* let execute_cycles cycles computer = *)
(* let half_cycles = 2 * cycles in *)
(* let rec aux n = function *)
(* | None -> printf "Empty Computer" *)
(* | Some c -> *)
(* if n = 0 *)
(* then dump_execution n (Some c) *)
(* else ( *)
(* let computer' = M.fetch_decode_execute c in *)
(* match computer' with *)
(* | None -> dump_execution (-1) None *)
(* | Some _ -> *)
(* dump_execution n computer'; *)
(* aux (n + 1) computer') *)
(* in *)
(* aux half_cycles computer *)
(* ;; *)

open Ocaml64.C6510.M

let init_test_computer =
  let mem = Array.create ~len:65536 0xFF in
  M.create mem
;;

(* let _dump_executions = List.iter ~f:dump_execution *)

(* let dump_last_execution executions = List.hd_exn executions |> dump_execution *)
let computer = init_test_computer
let mem = computer.banks

let computer =
  { computer with
    cpu = { computer.cpu with reset = false; pc = 0x400; address = 0x0400; data = 0xd8 }
  ; bus = { address = 0x0400; data = 0xD8 }
  }
;;

let load_bin_pgm pgm mem = String.iteri pgm ~f:(fun i byte -> mem.(i) <- Char.to_int byte)
let pgm = In_channel.read_all "program.bin"

(* let pgm1 = [ 0xA9; 0x01; 0xA2; 0x02; 0xA0; 0x03; 0x6C; 0x44; 0x69; 0xEA ] *)
(* let pgm1 = [ 0x00; 0xEA ] *)
let () = load_bin_pgm pgm mem

module type CircularBuffer = sig
  type 'a t

  val create : unit -> 'a t
  val add : 'a -> 'a t -> unit
  val to_list : 'a t -> 'a list
  (* val length : 'a t -> int *)

  (* val tail : 'a t -> int *)
  val head : 'a t -> int
  (* val capacity : int *)
end

module CircularBuffer : CircularBuffer = struct
  type 'a t =
    { data : 'a option array
    ; mutable head : int
    ; mutable tail : int
    }

  let head buffer = buffer.head

  (* let tail buffer = buffer.tail *)
  (* let capacity = 256 *)
  let capacity = 64
  let mask = capacity - 1

  (* let length buffer = buffer.head - buffer.tail *)
  let create () = { data = Array.create ~len:capacity None; head = 0; tail = 0 }

  let add item buffer =
    let index = buffer.head land mask in
    buffer.data.(index) <- Some item;
    buffer.head <- buffer.head + 1;
    if buffer.head - buffer.tail > capacity then buffer.tail <- buffer.head - capacity
  ;;

  let to_list buffer =
    let res = ref [] in
    for i = buffer.head - 1 downto buffer.tail do
      match buffer.data.(i land mask) with
      | None -> ()
      | Some x -> res := x :: !res
    done;
    !res
  ;;
end

type status =
  | OK
  | ERROR

type model =
  { running : bool
  ; tick : int
  ; step : int
  ; computer : M.t
  ; executions : (int * M.t) CircularBuffer.t
  ; status : status
  }

type msg =
  | Pause
  | Next
  | Quit
  | Tick of float
  | Step of int

let init () =
  let executions = CircularBuffer.create () in
  (* CircularBuffer.add computer executions; *)
  { running = false; step = 1; tick = 0; computer; executions; status = OK }, Cmd.none
;;

let loop model =
  let rec aux n model =
    if n = 0
    then model
    else (
      let tick = model.tick + 1 in
      match M.fetch_decode_execute model.computer with
      | None -> { model with status = ERROR }
      | Some computer ->
        CircularBuffer.add (tick, computer) model.executions;
        aux (n - 1) { model with tick; computer; status = OK })
  in
  aux model.step model
;;

let update msg model =
  match msg with
  | Tick _dt -> if model.running then loop model, Cmd.none else model, Cmd.none
  | Pause -> { model with running = not model.running }, Cmd.none
  | Next -> loop model, Cmd.none
  | Quit -> model, Cmd.quit
  | Step s when s >= 1 && s <= 9 -> { model with step = 10 ** (s - 1) }, Cmd.none
  | Step _ -> model, Cmd.none
;;

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let _columns =
  [ Table.column ~header:(Table.cell "Cycle") ~width:(`Fixed 16) ~justify:`Left "cycle/h"
  ; Table.column ~header:(Table.cell "AB") ~width:(`Fixed 6) ~justify:`Left "AB"
  ; Table.column ~header:(Table.cell "DB") ~width:(`Fixed 4) ~justify:`Right "DB"
  ; Table.column ~header:(Table.cell "PC") ~width:(`Fixed 6) ~justify:`Right "PC"
  ; Table.column ~header:(Table.cell "OP") ~width:(`Fixed 8) ~justify:`Center "OP"
  ; Table.column ~header:(Table.cell "A") ~width:(`Fixed 4) ~justify:`Center "A"
  ; Table.column ~header:(Table.cell "X") ~width:(`Fixed 4) ~justify:`Center "X"
  ; Table.column ~header:(Table.cell "Y") ~width:(`Fixed 4) ~justify:`Center "Y"
  ]
;;

let computer_to_row cycle (computer : M.t) =
  Printf.sprintf
    "%12d/%1d %4s %2s %04X %02X %04X %02X %02X %02X %02X %02X %8s %-16s"
    cycle
    (cycle land 1)
    (if computer.cpu.cycle = 1 then "SYNC" else "")
    (if computer.cpu.rw then "R" else "W")
    computer.bus.address
    computer.bus.data
    computer.cpu.pc
    computer.cpu.a
    computer.cpu.x
    computer.cpu.y
    computer.cpu.sp
    computer.cpu.sr
    (sr_to_string computer.cpu.sr)
    (inst_to_string computer.cpu.ir.inst computer.cpu.ir.mode computer.operand)
;;

let border_color = Ansi.Color.grayscale ~level:8

let header _model =
  box
    ~padding:(padding 1)
    ~background:header_bg
    [ box
        ~flex_direction:Row
        ~justify_content:Space_between
        ~align_items:Center
        ~size:{ width = pct 100; height = auto }
        [ text ~style:(Ansi.Style.make ~bold:true ()) "6502 Debug"
        ; text ~style:muted "▄▀ mosaic"
        ]
    ]
;;

let footer _model =
  box
    ~padding:(padding 1)
    ~background:footer_bg
    [ text ~style:hint "s cycle style  •  q quit" ]
;;

let trace model =
  box
    ~border:true
    ~title:"Trace"
    ~padding:(padding 1)
    ~size:{ width = pct 50; height = pct 100 }
    [ box
        ~flex_direction:Column
        ~border_color
        ~gap:(gap 0)
        ~size:{ width = pct 100; height = pct 100 }
        [ box
            ~flex_grow:0.
            ~padding:(padding 0)
            [ text "       Cycles/h SYNC RW AB   DB PC   A  X  Y  SP SR Flags    Asm" ]
        ; box
            ~flex_grow:0.
            ~padding:(padding 1)
            [ scroll_box
                ~scroll_y:true
                ~scroll_x:false
                ~sticky_scroll:true
                ~sticky_start:`Bottom
                ~size:{ width = pct 100; height = pct 100 }
                (let start = CircularBuffer.head model.executions in
                 List.mapi
                   (CircularBuffer.to_list model.executions)
                   ~f:(fun i (tick, computer) ->
                     let cycle = start + i in
                     box
                       ~key:(Int.to_string cycle)
                       ~padding:(padding 0)
                       ~background:
                         (if cycle % 2 = 0
                          then Ansi.Color.default
                          else Ansi.Color.grayscale ~level:3)
                       [ text (computer_to_row tick computer) ]))
            ]
        ]
    ]
;;

(* let trace model = *)
(* box *)
(* ~border:true *)
(* ~title:"Trace" *)
(* ~padding:(padding 1) *)
(* ~size:{ width = auto; height = pct 100 } *)
(* [ box *)
(* ~flex_direction:Column *)
(* ~gap:(gap 1) *)
(* ~size:{ width = pct 100; height = pct 100 } *)
(* [ box *)
(* ~flex_grow:1. *)
(* ~padding:(padding 1) *)
(* [ table *)
(* ~columns *)
(* ~rows: *)
(* (let rows = *)
(* List.map (CircularBuffer.to_list model.executions) ~f:(fun c -> *)
(* printf "%s\n" (cpu_to_string c.cpu); *)
(* computer_to_row model.tick (Some c)) *)
(* in *)
(* printf "Rows: %d\n" (List.length rows); *)
(* rows) *)
(* ~box_style:(style_to_prop model.style) *)
(* ~show_header:true *)
(* ~show_edge:true *)
(* ~show_lines:true *)
(* ~table_padding:(1, 1, 1, 1) *)
(* ~header_style:(Ansi.Style.make ~bold:true ()) *)
(* ~row_styles: *)
(* [ Ansi.Style.default *)
(* ; Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () *)
(* ] *)
(* () *)
(* ] *)
(* ] *)
(* ] *)
(* ;; *)

let cpu_controls model =
  box
    ~border:true
    ~title:"CPU Controls"
    ~padding:(padding 1)
    ~flex_direction:Column
    ~size:{ width = auto; height = pct 100 }
    [ box
        ~flex_direction:Column
        ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [ text (Printf.sprintf "Step size: %d" model.step) ]
    ; box
        ~flex_direction:Column
        ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [ box
            ~flex_direction:Row
            ~gap:(gap 1)
            [ box ~border:true ~padding:(padding 1) [ text "|<" ]
            ; box
                ~border:true
                ~padding:(padding 1)
                [ text (if model.running then "||" else "> ") ]
            ; box ~border:true ~padding:(padding 1) [ text ">|" ]
            ; box ~border:true ~padding:(padding 1) [ text ">>|" ]
            ; box ~border:true ~padding:(padding 1) [ text "->" ]
            ; box ~border:true ~padding:(padding 1) [ text "@" ]
            ]
        ]
    ; box
        ~flex_direction:Row
        ~gap:(gap 1)
        [ box
            ~padding:(padding 1)
            [ text
                (match model.status with
                 | OK -> "Status: OK"
                 | ERROR -> "Status: KO")
            ]
        ; box
            ~padding:(padding 1)
            [ text (Printf.sprintf "SR: %s" (sr_to_string model.computer.cpu.sr)) ]
        ; box
            ~padding:(padding 1)
            [ text (Printf.sprintf "Cycle: %d" model.computer.cpu.cycle) ]
        ]
    ; box
        ~flex_direction:Row
        ~gap:(gap 1)
        [ box
            ~padding:(padding 1)
            [ text
                (Printf.sprintf
                   "Addr: 0x%04X Data: 0x%02X PC: 0x%04X"
                   model.computer.cpu.address
                   model.computer.cpu.data
                   model.computer.cpu.pc)
            ]
        ]
    ; box
        ~flex_direction:Row
        ~gap:(gap 1)
        [ box
            ~padding:(padding 1)
            [ text
                (Printf.sprintf
                   "A: 0x%02X X: 0x%02X Y: 0x%02X"
                   model.computer.cpu.a
                   model.computer.cpu.x
                   model.computer.cpu.y)
            ]
        ]
    ; box
        ~flex_direction:Row
        ~gap:(gap 1)
        [ box
            ~padding:(padding 1)
            [ text
                (Printf.sprintf
                   "Phy1: %5b Phy2: %5b RDY: %5b"
                   model.computer.cpu.phy1
                   model.computer.cpu.phy2
                   model.computer.cpu.rdy)
            ]
        ]
    ]
;;

let view model =
  box
    ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [ (* Header *)
      header model (* content *)
    ; box
        ~flex_grow:1.
        ~flex_direction:Row
        ~gap:(gap 1)
        ~padding:(padding 1)
        [ trace model; cpu_controls model ]
    ; (* Footer *)
      footer model
    ]
;;

let subscriptions _model =
  Sub.batch
    [ Sub.on_key (fun ev ->
        match (Mosaic_ui.Event.Key.data ev).key with
        | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Next
        | Right -> Some Next
        | Char c when Uchar.equal c (Uchar.of_char '1') -> Some (Step 1)
        | Char c when Uchar.equal c (Uchar.of_char '2') -> Some (Step 2)
        | Char c when Uchar.equal c (Uchar.of_char '3') -> Some (Step 3)
        | Char c when Uchar.equal c (Uchar.of_char '4') -> Some (Step 4)
        | Char c when Uchar.equal c (Uchar.of_char '5') -> Some (Step 5)
        | Char c when Uchar.equal c (Uchar.of_char '6') -> Some (Step 6)
        | Char c when Uchar.equal c (Uchar.of_char '7') -> Some (Step 7)
        | Char c when Uchar.equal c (Uchar.of_char '8') -> Some (Step 8)
        | Char c when Uchar.equal c (Uchar.of_char '9') -> Some (Step 9)
        | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
        | Char c when Uchar.equal c (Uchar.of_char ' ') -> Some Pause
        | Escape -> Some Quit
        | _ -> None)
    ; Sub.on_tick (fun ~dt -> Tick dt)
    ]
;;

(* let () = *)
(* printf "%s\n" (cpu_to_string computer.cpu); *)
(* execute_cycles cycles (Some computer) *)
(* ;; *)

let () = run { init; update; view; subscriptions }
