open Base
open Stdio
open Ocaml64.C64
open Mosaic

let usage_msg = "program [--tui ] [--replay <replay_file>]"
let arg_tui = ref false
let arg_replay_file = ref ""

let speclist =
  [ "--tui", Stdlib.Arg.Set arg_tui, "Run TUI mode"
  ; "--replay", Stdlib.Arg.Set_string arg_replay_file, "Emulator replay file"
  ; "-r", Stdlib.Arg.Set_string arg_replay_file, "Emulator replay file"
  ]
;;

let anon_arg _ = ()
let () = Stdlib.Arg.parse speclist anon_arg usage_msg

open Ocaml64.C6510.M

module Trace = struct
  open Bigarray

  (* We will store 3 int64 for the whole computer state
      (without memory for the moment)
      and store them as consecutive int64 in a bigarray
      we use int64_elt and c_layout so that we store native
      int64 (and not the boxed ocaml version) and so that we can directly
      map to a file

      we use a double buffer to manage the history
      when a buffer is full, we compress it and flush it to disk
      and we swap the buffers
      
  *)
  type buffer_state =
    { mutable cursor : int
    ; data : (int64, int64_elt, c_layout) Array1.t
      (* Used to lock the buffer while compressing and dumpong to file
    to make sure we do not switch while compression is running
    in another thread *)
      (* ; busy : bool Atomic.t *)
    }

  type double_buffer =
    { mutable active : buffer_state
    ; mutable frozen : buffer_state
    ; max_cycles : int
    }

  (* let as_bigstring (arr : (int64, int64_elt, c_layout) Bigarray.Array1.t) *)
  (* : Core.Bigstring.t *)
  (* = *)
  (* let arr_len = Bigarray.Array1.dim arr in *)
  (* printf "as_bs: %d\n%!" arr_len; *)
  (* let reshaped = *)
  (* Bigarray.reshape_1 *)
  (* (Bigarray.genarray_of_array1 *)
  (* (Stdlib.Obj.magic arr : (char, int8_unsigned_elt, c_layout) Array1.t)) *)
  (* (arr_len ) *)
  (* in *)
  (* reshaped *)
  (* ;; *)

  let as_bigstring (arr : (int64, int64_elt, c_layout) Array1.t) =
    let len = Array1.dim arr in
    let b = Bigstringaf.create (len * 8) in
    for i = 0 to len - 1 do
      (* This writes the int64 into 8 bytes of the bigstring *)
      Bigstringaf.set_int64_be b (i * 8) (Array1.get arr i)
    done;
    b
  ;;

  let create max_cycles =
    let active =
      { cursor = 0; data = Array1.init int64 c_layout (max_cycles * 3) (fun _ -> 0L) }
    in
    let frozen = { cursor = 0; data = Array1.create int64 c_layout (max_cycles * 3) } in
    { max_cycles; active; frozen }
  ;;

  let compress_to_disk data =
    let filename = Printf.sprintf "trace_%f.bin.zst" (Unix.gettimeofday ()) in
    printf "Writing file data size %d %s\n%!" (Array1.size_in_bytes data) filename;
    let src = as_bigstring data in
    printf
      "Data length : data:%d data_as_bigstring:%d\n%!"
      (Array1.size_in_bytes data)
      (Core.Bigstring.length src);
    let input = Zstandard.Input.from_bigstring src in
    let size_limit =
      Zstandard.compression_output_size_bound (Core.Bigstring.length src |> Int64.of_int)
      |> Int64.to_int
    in
    let dst = Zstandard.Output.allocate_bigstring ~size_limit in
    let compressed_data =
      Zstandard.Simple.compress ~compression_level:1 ~input ~output:dst
    in
    let len = Core.Bigstring.length compressed_data in
    let fd = Unix.(openfile filename [ O_RDWR; O_CREAT; O_TRUNC ] 0o600) in
    let fdest =
      Bigarray.array1_of_genarray (Unix.map_file fd char c_layout true [| len |])
    in
    Core.Bigstring.blit ~src_pos:0 ~len ~src:compressed_data ~dst_pos:0 ~dst:fdest;
    Unix.close fd
  ;;

  let of_bigstring (bs : Bigstringaf.t) =
    let open Bigarray in
    let len = Bigstringaf.length bs in
    if len % 8 = 0
    then (
      let arrsize = len / 8 in
      let arr = Array1.create Int64 c_layout arrsize in
      for i = 0 to arrsize - 1 do
        (* This writes the int64 into 8 bytes of the bigstring *)
        let n = Bigstringaf.get_int64_be bs (i * 8) in
        Array1.set arr i n
      done;
      arr)
    else invalid_arg "Length should be a multiple of 8"
  ;;

  let decompress_from_disk filename =
    let fd = Unix.(openfile filename [ O_RDONLY ] 0o400) in
    let fdest =
      Bigarray.array1_of_genarray (Unix.map_file fd char c_layout false [| -1 |])
    in
    let input = Zstandard.Input.from_bigstring fdest in
    let len_in_bytes = Zstandard.decompressed_size input in
    printf "LEN_IN_BYTES = %s\n" (Int64.to_string len_in_bytes);
    let dst =
      Zstandard.Output.allocate_bigstring ~size_limit:(Int64.to_int len_in_bytes)
    in
    let decompressed_data = Zstandard.Simple.decompress ~input ~output:dst in
    let reshaped = of_bigstring decompressed_data in
    Unix.close fd;
    reshaped
  ;;

  let swap_buffers in_chan out_chan =
    let open Domainslib in
    while true do
      let t = Chan.recv in_chan in
      printf "swap_buffers: active: %d frozen: %d\n" t.active.cursor t.frozen.cursor;
      (* Loop until the buffer is not busy*)
      t.active.cursor <- 0;
      let temp = t.active in
      t.active <- t.frozen;
      t.frozen <- temp;
      Chan.send out_chan t;
      try
        (* Compress and dump *)
        (* printf "cOMPRESSING buffers\n"; *)
        compress_to_disk t.frozen.data
      with
      (* printf "cOMPRESSING buffers - AFER\n" *)
      | exn ->
        let msg = Exn.to_string exn in
        let backtrace = Backtrace.get () |> Backtrace.to_string in
        eprintf "Compression failed: %s\n%s%!" msg backtrace;
        failwith "Exception"
    done
  ;;

  let pack_computer (computer : M.t) =
    let pins =
      (if computer.cpu.rw then 0b1000_0000 else 0b0000_0000)
      lor (if computer.cpu.irq then 0b0100_0000 else 0b0000_0000)
      lor (if computer.cpu.nmi then 0b0010_0000 else 0b0000_0000)
      lor (if computer.cpu.aec then 0b0001_0000 else 0b0000_0000)
      lor (if computer.cpu.reset then 0b0000_1000 else 0b0000_0000)
      lor (if computer.cpu.phy1 then 0b0000_0100 else 0b0000_0000)
      lor (if computer.cpu.phy2 then 0b0000_0010 else 0b0000_0000)
      lor if computer.cpu.rdy then 0b0000_0001 else 0b0000_0000
    in
    let open Int64 in
    let pack1 =
      bit_or
        (shift_left (of_int computer.cpu.sp) 40)
        (bit_or
           (shift_left (of_int computer.cpu.a) 32)
           (bit_or
              (shift_left (of_int computer.cpu.x) 24)
              (bit_or
                 (shift_left (of_int computer.cpu.y) 16)
                 (bit_or
                    (shift_left (of_int computer.cpu.sr) 8)
                    (of_int computer.cpu.cycle)))))
    in
    let pack2 =
      bit_or
        (shift_left (of_int computer.cpu.data) 48)
        (bit_or
           (shift_left (of_int computer.cpu.pch) 40)
           (bit_or
              (shift_left (of_int computer.cpu.pcl) 32)
              (bit_or
                 (shift_left (of_int computer.cpu.address) 16)
                 (of_int computer.cpu.pc))))
    in
    (* TODO : Do not really need to stopre operand. SHould store opcode instead*)
    let pack3 =
      bit_or
        (shift_left (of_int computer.operand) 48)
        (bit_or
           (shift_left (of_int pins) 40)
           (bit_or
              (shift_left (0xFFL land of_int computer.cpu.ir.opcode) 32)
              (bit_or
                 (shift_left (of_int computer.cpu.ioport) 24)
                 (bit_or
                    (shift_left (of_int computer.bus.address) 8)
                    (of_int computer.bus.data)))))
    in
    pack1, pack2, pack3
  ;;

  let unpack_computer (pack1, pack2, pack3) =
    let open Int64 in
    let sp =
      shift_right (pack1 land shift_left 0xFFL 40) 40
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let a =
      shift_right (pack1 land shift_left 0xFFL 32) 32
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let x =
      shift_right (pack1 land shift_left 0xFFL 24) 24
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let y =
      shift_right (pack1 land shift_left 0xFFL 16) 16
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let sr =
      shift_right (pack1 land shift_left 0xFFL 8) 8
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let cycle = pack1 land 0x0FFL |> Int64.to_int |> Option.value ~default:0 in
    let data =
      shift_right (pack2 land shift_left 0xFFL 48) 48
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let pch =
      shift_right (pack2 land shift_left 0xFFL 40) 40
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let pcl =
      shift_right (pack2 land shift_left 0xFFL 32) 32
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let address =
      shift_right (pack2 land shift_left 0xFFFFL 16) 16
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let pc = pack2 land 0xFFFFL |> Int64.to_int |> Option.value ~default:0 in
    let operand =
      shift_right (pack3 land shift_left 0xFFFFL 48) 48
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let pins =
      shift_right (pack3 land shift_left 0xFFL 40) 40
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let opcode =
      shift_right (pack3 land shift_left 0xFFL 32) 32
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let ioport =
      shift_right (pack3 land shift_left 0xFFL 24) 24
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let bus_address =
      shift_right (pack3 land shift_left 0xFFFFL 8) 8
      |> Int64.to_int
      |> Option.value ~default:0
    in
    let bus_data = pack3 land 0xFFL |> Int64.to_int |> Option.value ~default:0 in
    let open Int in
    let rw = pins land 0b1000_0000 = 0b1000_0000 in
    let irq = pins land 0b0100_0000 = 0b0100_0000 in
    let nmi = pins land 0b0010_0000 = 0b0010_0000 in
    let aec = pins land 0b0001_0000 = 0b0001_0000 in
    let reset = pins land 0b0000_1000 = 0b0000_1000 in
    let phy1 = pins land 0b0000_0100 = 0b0000_0100 in
    let phy2 = pins land 0b0000_0010 = 0b0000_0010 in
    let rdy = pins land 0b0000_0001 = 0b0000_0001 in
    let cpu =
      { sp
      ; a
      ; x
      ; y
      ; sr
      ; cycle
      ; data
      ; pch
      ; pcl
      ; address
      ; ioport
      ; pc
      ; rw
      ; irq
      ; nmi
      ; aec
      ; reset
      ; phy1
      ; phy2
      ; rdy
      ; ir =
          (match decode opcode with
           | None ->
             raise (Invalid_argument (Printf.sprintf "Invalid opcode %02X" opcode))
           | Some ir -> ir)
      }
    in
    let bus = Ocaml64.C64.M.{ address = bus_address; data = bus_data } in
    bus, cpu, operand
  ;;

  let record to_swapper from_swapper t (previous : M.t option) (current : M.t) =
    let active = t.active in
    if active.cursor < t.max_cycles
    then (
      let index = t.active.cursor * 3 in
      let ppack1, ppack2, ppack3 =
        match previous with
        | None -> 0L, 0L, 0L
        | Some previous -> pack_computer previous
      in
      let pack1, pack2, pack3 = pack_computer current in
      let diff1, diff2, diff3 =
        Int64.bit_xor ppack1 pack1, Int64.bit_xor ppack2 pack2, Int64.bit_xor ppack3 pack3
      in
      active.data.{index} <- diff1;
      active.data.{index + 1} <- diff2;
      active.data.{index + 2} <- diff3;
      active.cursor <- active.cursor + 1)
    else (
      printf "Swapping %d\n%!" active.cursor;
      Domainslib.Chan.send to_swapper t;
      let _ = Domainslib.Chan.recv from_swapper in
      ())
  ;;
end

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
  ; memory_dump_start : int
  ; buffer : Trace.double_buffer
  ; to_swapper : Trace.double_buffer Domainslib.Chan.t
  ; from_swapper : Trace.double_buffer Domainslib.Chan.t
  }

type msg =
  | Pause
  | Next
  | Quit
  | Tick of float
  | Step of int
  | Set_memory_dump_start of string
  | Submit
  | Toggle_IRQ
  | Toggle_NMI
  | Toggle_RES
  | Toggle_RDY

let init () =
  let executions = CircularBuffer.create () in
  (* CircularBuffer.add computer executions; *)
  ( { running = false
    ; step = 1
    ; tick = 0
    ; computer
    ; executions
    ; status = OK
    ; memory_dump_start = 0x01FF
    ; buffer = Trace.create 2_000_000
    ; to_swapper = Domainslib.Chan.make_bounded 1
    ; from_swapper = Domainslib.Chan.make_bounded 1
    }
  , Cmd.none )
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
        Trace.record
          model.to_swapper
          model.from_swapper
          model.buffer
          (Some model.computer)
          computer;
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
  | Submit -> model, Cmd.none
  | Step s when s >= 1 && s <= 9 -> { model with step = 10 ** (s - 1) }, Cmd.none
  | Step _ -> model, Cmd.none
  | Set_memory_dump_start v ->
    let memory_dump_start =
      try Int.of_string v with
      | _ -> model.memory_dump_start
    in
    { model with memory_dump_start }, Cmd.none
  | Toggle_IRQ ->
    ( { model with
        computer =
          { model.computer with
            cpu = { model.computer.cpu with irq = not model.computer.cpu.irq }
          }
      }
    , Cmd.none )
  | Toggle_NMI ->
    ( { model with
        computer =
          { model.computer with
            cpu = { model.computer.cpu with nmi = not model.computer.cpu.nmi }
          }
      }
    , Cmd.none )
  | Toggle_RDY ->
    ( { model with
        computer =
          { model.computer with
            cpu = { model.computer.cpu with rdy = not model.computer.cpu.rdy }
          }
      }
    , Cmd.none )
  | Toggle_RES ->
    ( { model with
        computer =
          { model.computer with
            cpu = { model.computer.cpu with reset = not model.computer.cpu.reset }
          }
      }
    , Cmd.none )
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
    "%12d/%1d %4s %2s %04X %02X %04X %02X %02X %02X %02X %02X %8s %-11s"
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
            ~flex_grow:1.
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
                     (* Stdio.eprintf *)
                     (* "Start: %10d i: %4d cycle: %10d - %s\n" *)
                     (* start *)
                     (* i *)
                     (* cycle *)
                     (* (computer_to_row tick computer); *)
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
        ~gap:(gap 0)
        ~size:{ width = pct 100; height = auto }
        [ box
            ~flex_direction:Row
            ~gap:(gap 0)
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
        ~gap:(gap 0)
        ~padding:(padding 0)
        (* ~justify_items:Start *)
        [ box
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text "Status:"; text "Addr:"; text "Data:"; text "PC" ]
        ; box
            ~align_items:End
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text
                (match model.status with
                 | OK -> "OK"
                 | ERROR -> "KO")
            ; text (Printf.sprintf "0x%04X" model.computer.cpu.address)
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.data)
            ; text (Printf.sprintf "0x%04X" model.computer.cpu.pc)
            ]
        ; box
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text "Cycle:"; text "A:"; text "X:"; text "Y:" ]
        ; box
            ~align_items:End
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text (Printf.sprintf "%d" model.computer.cpu.cycle)
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.a)
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.x)
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.y)
            ]
        ; box
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text "ASM:"; text "FLags:"; text "SR:"; text "SP:" ]
        ; box
            ~align_items:End
            ~padding:(padding 1)
            ~flex_direction:Column
            [ text
                (Printf.sprintf
                   "%11s"
                   (inst_to_string
                      model.computer.cpu.ir.inst
                      model.computer.cpu.ir.mode
                      model.computer.operand))
            ; text (Printf.sprintf "%8s" (sr_to_string model.computer.cpu.sr))
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.sr)
            ; text (Printf.sprintf "0x%02X" model.computer.cpu.sp)
            ]
        ]
    ; box
        ~flex_direction:Row
        ~gap:(gap 1)
        [ box
            ~border:true
            ~padding:(padding 1)
            ~on_mouse:(fun ev ->
              match Event.Mouse.kind ev with
              | Down -> Some Toggle_RDY
              | _ -> None)
            [ text
                (Printf.sprintf "[%s] RDY" (if model.computer.cpu.rdy then "X" else " "))
            ]
        ; box
            ~border:true
            ~padding:(padding 1)
            ~on_mouse:(fun ev ->
              match Event.Mouse.kind ev with
              | Down -> Some Toggle_IRQ
              | _ -> None)
            [ text
                (Printf.sprintf "[%s] IRQ" (if model.computer.cpu.irq then "X" else " "))
            ]
        ; box
            ~border:true
            ~padding:(padding 1)
            ~on_mouse:(fun ev ->
              match Event.Mouse.kind ev with
              | Down -> Some Toggle_NMI
              | _ -> None)
            [ text
                (Printf.sprintf "[%s] NMI" (if model.computer.cpu.nmi then "X" else " "))
            ]
        ; box
            ~border:true
            ~padding:(padding 1)
            ~on_mouse:(fun ev ->
              match Event.Mouse.kind ev with
              | Down -> Some Toggle_RES
              | _ -> None)
            [ text
                (Printf.sprintf
                   "[%s] RES"
                   (if model.computer.cpu.reset then "X" else " "))
            ]
        ]
    ]
;;

let memory_to_row8 start mem =
  let rec loop acc n =
    if n = 0
    then acc
    else (
      let start_row_addr = start + (8 * (n - 1)) in
      loop
        (Printf.sprintf
           "%04X %02X %02X %02X %02X %02X %02X %02X %02X"
           start_row_addr
           mem.(start_row_addr)
           mem.(start_row_addr + 1)
           mem.(start_row_addr + 2)
           mem.(start_row_addr + 3)
           mem.(start_row_addr + 4)
           mem.(start_row_addr + 5)
           mem.(start_row_addr + 6)
           mem.(start_row_addr + 7)
         :: acc)
        (n - 1))
  in
  loop [] 32
;;

let memory model =
  box
    ~border:true
    ~flex_direction:Column
    ~title:"Memory"
    ~padding:(padding 1)
    ~size:{ width = auto; height = pct 100 }
    [ box
        ~flex_direction:Row
        ~align_items:Center
        ~gap:(gap 1)
        [ box ~size:{ width = auto; height = px 1 } [ text "Memory Address:" ]
        ; input
            ~id:"Memory Start"
            ~placeholder:"Enter Memory address (hex)"
            ~size:{ width = px 25; height = px 1 }
            ~value:(Int.to_string model.memory_dump_start)
            ~on_input:(fun v -> Some (Set_memory_dump_start v))
            ()
        ]
    ; box
        ~flex_direction:Column
        ~border_color
        ~gap:(gap 0)
        ~size:{ width = pct 100; height = pct 100 }
        [ box
            ~flex_grow:1.
            ~padding:(padding 1)
            [ scroll_box
                ~scroll_y:true
                ~scroll_x:false
                ~size:{ width = pct 100; height = pct 100 }
                (let mem_rows = memory_to_row8 0x0100 model.computer.banks in
                 List.map mem_rows ~f:(fun str -> box ~padding:(padding 0) [ text str ]))
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
        [ trace model; cpu_controls model; memory model ]
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
        | Enter -> Some Submit
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

(* let buff = Trace.create 2_000_000 *)
let buff = Trace.create 1_000_000

let dump_execution n (previous : M.t option) (computer : M.t) =
  match previous with
  | None -> failwith "Impossible"
  | Some previous ->
    let ppack1, ppack2, ppack3 = Trace.pack_computer previous in
    let pack1, pack2, pack3 = Trace.pack_computer computer in
    printf
      (* "%10d %016LX %016LX %016LX %016LX %016LX ab: 0x%04X db: 0x%02X %s\n" *)
      "%10d %016LX %016LX %016LX ab: 0x%04X db: 0x%02X %s\n"
      (* "%10d ab: 0x%04X db: 0x%02X %s\n" *)
      n
      (* (Int64.to_string pack1) *)
      (* ppack3 *)
      (* pack3 *)
      (Int64.bit_xor ppack1 pack1)
      (Int64.bit_xor ppack2 pack2)
      (Int64.bit_xor ppack3 pack3)
      computer.bus.address
      computer.bus.data
      (Ocaml64.C6510.M.cpu_to_string computer.cpu computer.operand)
;;

(* Trace.record buff previous computer *)
open Domainslib

(* Create an emty computer with all zeroes so that when we
xor it with the initial computer we get the computer *)

let execute_cycles to_swapper from_swapper cycles computer =
  let half_cycles = 2 * cycles in
  let rec aux n c =
    if n = 0
    then (
      (* TODO: CHeck if we really need this when we exit *)
      (* We need to make sure we save the last state *)
      Chan.send to_swapper buff;
      (*TODO receive should get the status *)
      let _ = Chan.recv from_swapper in
      ())
    else (
      let computer' = M.fetch_decode_execute c in
      match computer' with
      | None -> failwith "TODO: Received Non computer\n"
      (* TODO: CHeck fi we really need this when we exit *)
      (* Chan.send to_swapper buff; *)
      (* let _ = Chan.recv from_swapper in *)
      (* () *)
      | Some c' ->
        dump_execution n (Some c) c';
        Trace.record to_swapper from_swapper buff (Some c) c';
        aux (n - 1) c')
  in
  (* dump_execution 0 None computer; *)
  Trace.record to_swapper from_swapper buff None computer;
  aux half_cycles computer;
  Chan.send to_swapper buff;
  (*TODO receive should get the status *)
  let _ = Chan.recv from_swapper in
  ()
;;

let () =
  if String.equal !arg_replay_file ""
  then
    if !arg_tui
    then run { init; update; view; subscriptions }
    else (
      (* printf "%s\n" (cpu_to_string computer.cpu); *)
      printf "Starting\n";
      let to_swapper = Chan.make_bounded 1 in
      let from_swapper = Chan.make_bounded 1 in
      printf "Spawning swap buffer domain\n";
      let _ = Domain.spawn (fun () -> Trace.swap_buffers to_swapper from_swapper) in
      execute_cycles to_swapper from_swapper 500_000_000 computer)
  else (
    let uncompressed_data = Trace.decompress_from_disk !arg_replay_file in
    let len = Bigarray.Array1.dim uncompressed_data in
    let prev1 = ref 0L in
    let prev2 = ref 0L in
    let prev3 = ref 0L in
    prev1 := uncompressed_data.{0};
    prev2 := uncompressed_data.{1};
    prev3 := uncompressed_data.{2};
    let num_records = len / 3 in
    for i = 1 to num_records - 1 do
      (* Trace.unpack_computer (init1) *)
      let u1 = Int64.bit_xor uncompressed_data.{3 * i} !prev1 in
      let u2 = Int64.bit_xor uncompressed_data.{(3 * i) + 1} !prev2 in
      let u3 = Int64.bit_xor uncompressed_data.{(3 * i) + 2} !prev3 in
      let _, cpu, operand = Trace.unpack_computer (u1, u2, u3) in
      printf "%016LX %016LX %016LX\t" u1 u2 u3;
      printf "%s\n" (cpu_to_string cpu operand);
      prev1 := u1;
      prev2 := u2;
      prev3 := u3
    done)
;;
