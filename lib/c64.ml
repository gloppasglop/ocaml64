open Base
open Stdio

module M = struct
  open C6510.M

  type bus =
    { data : int
    ; address : int
    }

  type memory_banks = int array

  type t =
    { cpu : cpu
    ; bus : bus
    ; banks : memory_banks
    }

  let create banks =
    { cpu =
        { rdy = true
        ; irq = false
        ; nmi = false
        ; phy1 = true
        ; phy2 = false
        ; aec = true
        ; rw = true
        ; reset = false
        ; address = 0x00FF
        ; data = 0x00
        ; ioport = 0
        ; a = 0
        ; x = 0
        ; y = 0
        ; sr = 0b0000_0010
        ; pc = 0x0FF
        ; sp = 0xFF
        ; ir = decode 0x00
        ; cycle = 1
        ; pcl = 0
        ; pch = 0
        }
        (* End of CPU record *)
    ; bus = { address = 0; data = 0 }
    ; banks
    }
  ;;

  (* This is now wrong! *)

  (* module C64 : Computer = struct *)

  (* type memory_banks = int array *)

  (* type cpu = Cpu.C6510 *)

  (* in the future there will be multiple memory banks *)

  (* type t = *)
  (* { cpu : cpu *)
  (* ; bus : bus *)
  (* ; banks : memory_banks *)
  (* } *)

  (* let create banks = *)
  (* let cpu = *)
  (* { (* Pins *) *)
  (* rdy = true (* Ready *) *)
  (* ; irq = false (* IRQ - Inverted *) *)
  (* ; nmi = false (* Non Maskable Interrrupt - Inverted *) *)
  (* ; phy1 = true (* phy1 , IN*) *)
  (* ; phy2 = true (* phy2 , OUT*) *)
  (* ; aec = true (* Address Enable Control *) *)
  (* ; rw = true (* R/W Read/Write *) *)
  (* ; reset = false (* Reset - Inverted *) *)
  (* ; address = 0x00FF (* Pins A0 to A15 *) *)
  (* ; data = 0x00 (* Pins DB0 to DB7 *) *)
  (* ; ioport = 0 (* Pins P0 to  P7 *) *)
  (* ; a = 0x00 (* A register *) *)
  (* ; x = 0 (* X register *) *)
  (* ; y = 0 (* Y register *) *)
  (* ; sr = 0b0000_0010 (* Status register *) *)
  (* ; pc = 0x00FF (* Program counter *) *)
  (* ; sp = 0x00 (* Stack pointer *) *)
  (* ; ir = *)
  (* decode 0x00 *)
  (* Internal instruction register TODO: Should it be directly teh decoded instruction *)
  (* ; cycle = 1 *)
  (* } *)
  (* in *)
  (* { cpu; bus = { address = 0; data = 0 }; memory_banks = banks } *)
  (* ;; *)

  let fetch_decode_execute { cpu; bus; banks } =
    (* During the first cycle of in struction we need to read from memory we take the value on the bus put it on the data pins of the cpu *)
    (* TODO: Add a mapping function to access the correct bank *)
    let phy2 = cpu.phy2 in
    let computer =
      let mem = banks in
      match phy2 with
      | false ->
        let bus' = { bus with address = cpu.address } in
        let cpu' = { cpu with phy2 = true } in
        { cpu = cpu'; bus = bus'; banks }
      | true ->
        (match cpu.rdy with
         | true ->
           let cpu' = tick { cpu with data = bus.data; address = bus.address } in
           let bus' =
             match cpu'.rw with
             | true -> { address = cpu'.address; data = mem.(cpu'.address) }
             | false ->
               mem.(cpu'.address) <- cpu'.data;
               { address = cpu'.address; data = cpu'.data }
           in
           { cpu = { cpu' with phy2 = false; address = bus'.address; data = bus'.data }
           ; bus = bus'
           ; banks
           }
         | false -> { cpu; bus; banks = mem })
    in
    computer
  ;;
end

let load_pgm mem offset pgm = List.iteri pgm ~f:(fun i data -> mem.(offset + i) <- data)

let execute_cycles cycles computer =
  let half_cycles = 2 * cycles in
  let rec aux n acc computer =
    if n = 0
    then computer :: acc
    else (
      let computer' = M.fetch_decode_execute computer in
      aux (n - 1) (computer :: acc) computer')
  in
  aux half_cycles [] computer
;;

let program_start = 0x1000

let init_test_computer pgm =
  let mem = Array.create ~len:65536 0xFF in
  let address = program_start in
  let data = List.hd_exn pgm in
  let bus = M.{ data; address } in
  load_pgm mem program_start pgm;
  let computer = M.create mem in
  { computer with cpu = { computer.cpu with pc = address; address; data }; bus }
;;

let dump_execution (computer : M.t) =
  printf
    "ab: 0x%04X db: 0x%02X %s\n"
    computer.bus.address
    computer.bus.data
    (C6510.M.cpu_to_string computer.cpu)
;;

let dump_executions = List.iter ~f:dump_execution
let dump_last_execution executions = List.hd_exn executions |> dump_execution

module Cpu = C6510.M

let%expect_test "testing NOP IMPLIED (0xEA)" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xEA ] in
  let computer = init_test_computer pgm in
  (* printf "%s" (C6510.M.cpu_to_string computer.cpu); *)
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: NOP |}]
;;

let%expect_test "testing LDA IMMEDIATE (0xA9) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x01 ] in
  let computer = init_test_computer pgm in
  (* printf "%s" (Cpu.cpu_to_string computer.cpu); *)
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDA #%02X |}]
;;

let%expect_test "testing LDA IMMEDIATE (0xA9) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x81 ] in
  let computer = init_test_computer pgm in
  (* printf "%s" (Cpu.cpu_to_string computer.cpu); *)
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x81 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDA #%02X |}]
;;

let%expect_test "testing LDA IMMEDIATE (0xA9) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDA #%02X |}]
;;

let%expect_test "testing LDX IMMEDIATE (0xA2) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x01 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x01 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDX #%02X |}]
;;

let%expect_test "testing LDX IMMEDIATE (0xA2) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x81 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x81 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDX #%02X |}]
;;

let%expect_test "testing LDX IMMEDIATE (0xA2) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with x = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDX #%02X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xA0) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x01 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x01 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDY #%02X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xA0) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x81 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x81 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDY #%02X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xA0) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with y = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDY #%02X |}]
;;

let%expect_test "testing LDA ZEROPAGE (0xA5) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA5; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDA $%02X |}]
;;

let%expect_test "testing LDA ZEROPAGE (0xA5) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA5; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDA $%02X |}]
;;

let%expect_test "testing LDA ZEROPAGE (0xA5) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA5; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDA $%02X |}]
;;

let%expect_test "testing LDX ZEROPAGE (0xA6) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x01 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDX $%02X |}]
;;

let%expect_test "testing LDX ZEROPAGE (0xA6) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x80 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDX $%02X |}]
;;

let%expect_test "testing LDX ZEROPAGE (0xA6) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDX $%02X |}]
;;

let%expect_test "testing LDY ZEROPAGE (0xA4) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA4; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x01 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LDY $%02X |}]
;;

let%expect_test "testing LDY ZEROPAGE (0xA4) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA4; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x80 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: LDY $%02X |}]
;;

let%expect_test "testing LDY ZEROPAGE (0xA4) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xA4; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x044) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: LDY $%02X |}]
;;

let%expect_test "testing EOR ZEROPAGE (0x45) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x45; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0b0000_0001;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: EOR $%02X |}]
;;

let%expect_test "testing EOR ZEROPAGE (0x45) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x45; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: EOR $%02X |}]
;;

let%expect_test "testing EOR ZEROPAGE (0x45) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x45; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b0000_0010 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: EOR $%02X |}]
;;

let%expect_test "testing AND ZEROPAGE (0x25) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x25; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0b0000_0011;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: AND $%02X |}]
;;

let%expect_test "testing AND ZEROPAGE (0x25) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x25; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: AND $%02X |}]
;;

let%expect_test "testing AND ZEROPAGE (0x25) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x25; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0001; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b0000_0010 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: AND $%02X |}]
;;

let%expect_test "testing ORA ZEROPAGE (0x05) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x05; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0b0000_0011;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ORA $%02X |}]
;;

let%expect_test "testing ORA ZEROPAGE (0x05) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x05; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ORA $%02X |}]
;;

let%expect_test "testing ORA ZEROPAGE (0x05) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x05; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0000; sr = 0x02 } }
  in
  computer.banks.(0x044) <- 0b0000_0000;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: ORA $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) No flags" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) with incomming Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x01 } } in
  computer.banks.(0x44) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x06 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) Generating Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  computer.banks.(0x44) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) Pos+Pos=Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdizc pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) Neg+Neg=Pos " =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x80; sr = 0x80 } } in
  computer.banks.(0x44) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sp: 0xFF sr: nV-bdizC pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing ADC Binary ZEROPAGE (0x65) Pos+Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x65; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ADC $%02X |}]
;;

let%expect_test "testing STA ZEROPAGE (0x85)" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x85; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: STA $%02X

    Mem: 0x0044 : 0x01
    |}]
;;

let%expect_test "testing STX ZEROPAGE (0x86)" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x86; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: STX $%02X

    Mem: 0x0044 : 0x02
    |}]
;;

let%expect_test "testing STY ZEROPAGE (0x84)" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0x84; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: STY $%02X

    Mem: 0x0044 : 0x03
    |}]
;;

let%expect_test "testing ASL ZEROPAGE Basic (0x06)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x06; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ASL $%02X

    Mem: 0x0044 : 0x02
    |}]
;;

let%expect_test "testing ASL ZEROPAGE Shift Out (0x06)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x06; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: ASL $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing ASL ZEROPAGE Negative FLag  (0x06)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x06; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x40;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ASL $%02X

    Mem: 0x0044 : 0x80
    |}]
;;

let%expect_test "testing LSR ZEROPAGE Basic (0x46)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x46; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x02;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LSR $%02X

    Mem: 0x0044 : 0x01
    |}]
;;

let%expect_test "testing LSR ZEROPAGE Shift Into (0x46)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x46; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: LSR $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing LSR ZEROPAGE Hign Bit clear  (0x46)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x46; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: LSR $%02X

    Mem: 0x0044 : 0x40
    |}]
;;

let%expect_test "testing ROR ZEROPAGE Carry to Bit 7 (0x66)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x66; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ROR $%02X

    Mem: 0x0044 : 0x80
    |}]
;;

let%expect_test "testing ROR ZEROPAGE Bit 0 to Carry (0x66)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x66; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: ROR $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing ROR ZEROPAGE Negative Flag (0x66)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x66; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x7F;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: ROR $%02X

    Mem: 0x0044 : 0xBF
    |}]
;;

let%expect_test "testing ROL ZEROPAGE Carry to bit 0 (0x26)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x26; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ROL $%02X

    Mem: 0x0044 : 0x01
    |}]
;;

let%expect_test "testing ROL ZEROPAGE Bit 7 to Carry (0x26)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x26; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: ROL $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing ROL ZEROPAGE Negative Flag (0x26)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x26; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x40;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ROL $%02X

    Mem: 0x0044 : 0x80
    |}]
;;

let%expect_test "testing INC ZEROPAGE Add 1 (0xE6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xE6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: INC $%02X

    Mem: 0x0044 : 0x02
    |}]
;;

let%expect_test "testing INC ZEROPAGE Negative Flag (0xE6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xE6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x7F;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: INC $%02X

    Mem: 0x0044 : 0x80
    |}]
;;

let%expect_test "testing INC ZEROPAGE OverFlow (0xE6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xE6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0xFF;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: INC $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing DEC ZEROPAGE dec 1 (0xC6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xC6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x02;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: DEC $%02X

    Mem: 0x0044 : 0x01
    |}]
;;

let%expect_test "testing DEC ZEROPAGE set Negative Flag (0xC6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xC6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x02 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: DEC $%02X

    Mem: 0x0044 : 0xFF
    |}]
;;

let%expect_test "testing DEC ZEROPAGE Unset negative (0xC6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xC6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: DEC $%02X

    Mem: 0x0044 : 0x7F
    |}]
;;

let%expect_test "testing DEC ZEROPAGE dec to zero (0xC6)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0xC6; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x44 last_computer.banks.(0x44);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: DEC $%02X

    Mem: 0x0044 : 0x00
    |}]
;;

let%expect_test "testing SBC Binary ZEROPAGE (0xE5) No borrow" =
  (* 2 cycles , 1 byte *)
  let cycles = 3 in
  (* LDA $44 *)
  let pgm = [ 0xE5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x05; sr = 0x01 } } in
  computer.banks.(0x44) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizC pc: 0x1002 inst: SBC $%02X |}]
;;

let%expect_test "testing SBC Binary ZEROPAGE (0xE5) Substraction with Borrow " =
  let cycles = 3 in
  let pgm = [ 0xE5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x05; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizC pc: 0x1002 inst: SBC $%02X |}]
;;

let%expect_test "testing SBC Binary ZEROPAGE (0xE5) Underflow " =
  let cycles = 3 in
  let pgm = [ 0xE5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x01 } } in
  computer.banks.(0x44) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: SBC $%02X |}]
;;

let%expect_test "testing SBC Binary ZEROPAGE (0xE5) Overflow " =
  let cycles = 3 in
  let pgm = [ 0xE5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x80; sr = 0x01 } } in
  computer.banks.(0x44) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sp: 0xFF sr: nV-bdizC pc: 0x1002 inst: SBC $%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC5) Equality " =
  let cycles = 3 in
  let pgm = [ 0xC5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x42; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x42 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CMP $%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC5) Greater than" =
  let cycles = 3 in
  let pgm = [ 0xC5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0xFF; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CMP $%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC5) less than" =
  let cycles = 3 in
  let pgm = [ 0xC5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CMP $%02X |}]
;;

let%expect_test "testing BIT ZEROPAGE (0x24) specific bit" =
  let cycles = 3 in
  let pgm = [ 0x24; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x08; sr = 0x00 } } in
  computer.banks.(0x44) <- 0x0F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x08 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: BIT $%02X |}]
;;

let%expect_test "testing BIT ZEROPAGE (0x24) Negative/Overflow" =
  let cycles = 3 in
  let pgm = [ 0x24; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x00; sr = 0x02 } } in
  computer.banks.(0x44) <- 0xC0;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdiZc pc: 0x1002 inst: BIT $%02X |}]
;;

let%expect_test "testing BIT ZEROPAGE (0x24) Masking" =
  let cycles = 3 in
  let pgm = [ 0x24; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  computer.banks.(0x44) <- 0xFE;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdiZc pc: 0x1002 inst: BIT $%02X |}]
;;

let%expect_test "testing CPX ZEROPAGE (0xE4) Equality " =
  let cycles = 3 in
  let pgm = [ 0xE4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x42; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x42 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CPX $%02X |}]
;;

let%expect_test "testing CPX ZEROPAGE (0xE4) Greater than" =
  let cycles = 3 in
  let pgm = [ 0xE4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CPX $%02X |}]
;;

let%expect_test "testing CPX ZEROPAGE (0xE4) less than" =
  let cycles = 3 in
  let pgm = [ 0xE4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CPX $%02X |}]
;;

let%expect_test "testing CPY ZEROPAGE (0xC4) Equality " =
  let cycles = 3 in
  let pgm = [ 0xC4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x42; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x42 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CPY $%02X |}]
;;

let%expect_test "testing CPY ZEROPAGE (0xC4) Greater than" =
  let cycles = 3 in
  let pgm = [ 0xC4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CPY $%02X |}]
;;

let%expect_test "testing CPY ZEROPAGE (0xC4) less than" =
  let cycles = 3 in
  let pgm = [ 0xC4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sr = 0x00 } }
  in
  computer.banks.(0x44) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x02 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CPY $%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) No flags" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) with incomming Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x01 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x06 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) Generating Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0xFF ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) Pos+Pos=Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdizc pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) Neg+Neg=Pos " =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0xFF ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x80; sr = 0x80 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sp: 0xFF sr: nV-bdizC pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing ADC Binary IMMEDIATE (0x69) Pos+Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x69; 0x80 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ADC #%02X |}]
;;

let%expect_test "testing AND IMMEDIATE (0x29) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x29; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: AND #%02X |}]
;;

let%expect_test "testing AND IMMEDIATE (0x29) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x29; 0x81 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: AND #%02X |}]
;;

let%expect_test "testing AND IMMEDIATE (0x29) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x29; 0x02 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0001; sr = 0x00 } }
  in
  computer.banks.(0x044) <- 0b0000_0010 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: AND #%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC9) Equality " =
  let cycles = 2 in
  let pgm = [ 0xC9; 0x42 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x42; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x42 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CMP #%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC9) Greater than" =
  let cycles = 2 in
  let pgm = [ 0xC9; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0xFF; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CMP #%02X |}]
;;

let%expect_test "testing CMP ZEROPAGE (0xC9) less than" =
  let cycles = 2 in
  let pgm = [ 0xC9; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CMP #%02X |}]
;;

let%expect_test "testing CPX IMMEDIATE (0xE0) Equality " =
  let cycles = 2 in
  let pgm = [ 0xE0; 0x42 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x42; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x42 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CPX #%02X |}]
;;

let%expect_test "testing CPX IMMEDIATE (0xE0) Greater than" =
  let cycles = 2 in
  let pgm = [ 0xE0; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CPX #%02X |}]
;;

let%expect_test "testing CPX IMMEDIATE (0xE0) less than" =
  let cycles = 2 in
  let pgm = [ 0xE0; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CPX #%02X |}]
;;

let%expect_test "testing CPY IMMEDIATE (0xC0) Equality " =
  let cycles = 2 in
  let pgm = [ 0xC0; 0x42 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x42; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x42 sp: 0xFF sr: nv-bdiZC pc: 0x1002 inst: CPY #%02X |}]
;;

let%expect_test "testing CPY IMMEDIATE (0xC0) Greater than" =
  let cycles = 2 in
  let pgm = [ 0xC0; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFF sr: Nv-bdizC pc: 0x1002 inst: CPY #%02X |}]
;;

let%expect_test "testing CPY IMMEDIATE (0xC0) less than" =
  let cycles = 2 in
  let pgm = [ 0xC0; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x02 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: CPY #%02X |}]
;;

let%expect_test "testing EOR IMMEDIATE (0x49) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x49; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: EOR #%02X |}]
;;

let%expect_test "testing EOR IMMEDIATE (0x49) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x49; 0x81 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: EOR #%02X |}]
;;

let%expect_test "testing EOR IMMEDIATE (0x49) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x49; 0x02 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: EOR #%02X |}]
;;

let%expect_test "testing ORA IMMEDIATE (0x09) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x09; 0x03 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1002 inst: ORA #%02X |}]
;;

let%expect_test "testing ORA IMMEDIATE (0x09) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x09; 0x81 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1002 inst: ORA #%02X |}]
;;

let%expect_test "testing ORA IMMEDIATE (0x09) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  (* LDA $44 *)
  let pgm = [ 0x09; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0000; sr = 0x02 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1002 inst: ORA #%02X |}]
;;

let%expect_test "testing LDA ABSOLUTE (0xAD) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xAD; 0x69; 0x42 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4269) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: LDA $%04X |}]
;;

let%expect_test "testing LDA ABSOLUTE (0xAD) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xAD; 0x69; 0x42 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4269) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: LDA $%04X |}]
;;

let%expect_test "testing LDA ABSOLUTE (0xAD) zero" =
  let cycles = 4 in
  let pgm = [ 0xAD; 0x69; 0x42 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4269) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: LDA $%04X |}]
;;

let%expect_test "testing LDX ABSOLUTE (0xAE) non-zero positive" =
  let cycles = 4 in
  (*69 LDA $44 *)
  let pgm = [ 0xAE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x01 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: LDX $%04X |}]
;;

let%expect_test "testing LDX ABSOLUTE (0xAE) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xAE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x80 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: LDX $%04X |}]
;;

let%expect_test "testing LDX ABSOLUTE (0xAE) zero" =
  let cycles = 4 in
  let pgm = [ 0xAE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: LDX $%04X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xAC) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xAC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x04469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x01 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: LDY $%04X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xAC) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xAC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x04469) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x80 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: LDY $%04X |}]
;;

let%expect_test "testing LDY IMMEDIATE (0xAC) zero" =
  let cycles = 4 in
  let pgm = [ 0xAC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x04469) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: LDY $%04X |}]
;;

let%expect_test "testing ORA ABSOLUTE (0x0D) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x0D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0b0000_0011;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: ORA $%04X |}]
;;

let%expect_test "testing ORA ABSOLUTE (0x0D) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x0D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  computer.banks.(0x04469) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: ORA $%04X |}]
;;

let%expect_test "testing ORA ABSOLUTE (0x0D) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x0D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0000; sr = 0x02 } }
  in
  computer.banks.(0x04469) <- 0b0000_0000;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: ORA $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) No flags" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) with incomming Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x01 } } in
  computer.banks.(0x4469) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x06 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) Generating Carry " =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) Pos+Pos=Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdizc pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) Neg+Neg=Pos " =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x80; sr = 0x80 } } in
  computer.banks.(0x4469) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sp: 0xFF sr: nV-bdizC pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing ADC Binary ABSOLUTE (0x6D) Pos+Neg " =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x6D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x7F; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: ADC $%04X |}]
;;

let%expect_test "testing AND ABSOLUTE (0x2D) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x2D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0b0000_0011;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: AND $%04X |}]
;;

let%expect_test "testing AND ABSOLUTE (0x2D) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x2D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b1000_0010; sr = 0x00 } }
  in
  computer.banks.(0x04469) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: AND $%04X |}]
;;

let%expect_test "testing AND ABSOLUTE (0x2D) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x2D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0001; sr = 0x00 } }
  in
  computer.banks.(0x04469) <- 0b0000_0010 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: AND $%04X |}]
;;

let%expect_test "testing BIT ABSOLUTE (0x2C) specific bit" =
  let cycles = 4 in
  let pgm = [ 0x2C; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x08; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x0F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x08 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: BIT $%04X |}]
;;

let%expect_test "testing BIT ABSOLUTE (0x2C) Negative/Overflow" =
  let cycles = 4 in
  let pgm = [ 0x2C; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x00; sr = 0x02 } } in
  computer.banks.(0x4469) <- 0xC0;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdiZc pc: 0x1003 inst: BIT $%04X |}]
;;

let%expect_test "testing BIT ABSOLUTE (0x2C) Masking" =
  let cycles = 4 in
  let pgm = [ 0x2C; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0xFE;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: NV-bdiZc pc: 0x1003 inst: BIT $%04X |}]
;;

let%expect_test "testing CMP ABSOLUTE (0xCD) Equality " =
  let cycles = 4 in
  let pgm = [ 0xCD; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x42; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x42 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: CMP $%04X |}]
;;

let%expect_test "testing CMP ABSOLUTE (0xCD) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xCD; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0xFF; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizC pc: 0x1003 inst: CMP $%04X |}]
;;

let%expect_test "testing CMP ABSOLUTE (0xCD) less than" =
  let cycles = 4 in
  let pgm = [ 0xCD; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x02; sr = 0x00 } } in
  computer.banks.(0x4469) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: CMP $%04X |}]
;;

let%expect_test "testing CPX ABSOLUTE (0xEC) Equality " =
  let cycles = 4 in
  let pgm = [ 0xEC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x42; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x42 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: CPX $%04X |}]
;;

let%expect_test "testing CPX ABSOLUTE (0xEC) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xEC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1003 inst: CPX $%04X |}]
;;

let%expect_test "testing CPX ZEROPAGE (0xEC) less than" =
  let cycles = 4 in
  let pgm = [ 0xEC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: CPX $%04X |}]
;;

let%expect_test "testing CPY ABSOLUTE (0xCC) Equality " =
  let cycles = 4 in
  let pgm = [ 0xCC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x42; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x42 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: CPY $%04X |}]
;;

let%expect_test "testing CPY ABSOLUTE (0xCC) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xCC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFF sr: Nv-bdizC pc: 0x1003 inst: CPY $%04X |}]
;;

let%expect_test "testing CPY ABSOLUTE (0xCC) less than" =
  let cycles = 4 in
  let pgm = [ 0xCC; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x02 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: CPY $%04X |}]
;;

let%expect_test "testing EOR ABSOLUTE (0x4D) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x4D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x4469) <- 0b0000_0001;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: EOR $%04X |}]
;;

let%expect_test "testing EOR ABSOLUTE (0x4D) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x4D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x04469) <- 0b1000_0001 (* 0x81 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: EOR $%04X |}]
;;

let%expect_test "testing EOR ABSOLUTE (0x4D) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x4D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0b0000_0010; sr = 0x00 } }
  in
  computer.banks.(0x04469) <- 0b0000_0010 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: EOR $%04X |}]
;;

let%expect_test "testing STA IMMEDIATE (0x8D)" =
  let cycles = 4 in
  let pgm = [ 0x8D; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: STA $%04X

    Mem: 0x4469 : 0x01
    |}]
;;

let%expect_test "testing STX IMMEDIATE (0x8E)" =
  let cycles = 4 in
  let pgm = [ 0x8E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: STX $%04X

    Mem: 0x4469 : 0x02
    |}]
;;

let%expect_test "testing STY IMMEDIATE (0x8C)" =
  let cycles = 4 in
  let pgm = [ 0x8C; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: STY $%04X

    Mem: 0x4469 : 0x03
    |}]
;;

let%expect_test "testing DEC ABSOLUTE dec 1 (0xCE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xCE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x02;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: DEC $%04X

    Mem: 0x4469 : 0x01
    |}]
;;

let%expect_test "testing DEC ABSOLUTE set Negative Flag (0xCE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xCE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x02 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: DEC $%04X

    Mem: 0x4469 : 0xFF
    |}]
;;

let%expect_test "testing DEC ABSOLUTE Unset negative (0xCE)" =
  let cycles = 6 in
  let pgm = [ 0xCE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: DEC $%04X

    Mem: 0x4469 : 0x7F
    |}]
;;

let%expect_test "testing DEC ABSOLUTE dec to zero (0xCE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xCE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: DEC $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing ASL IMMEDIATE Basic (0x0E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x0E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: ASL $%04X

    Mem: 0x4469 : 0x02
    |}]
;;

let%expect_test "testing ASL IMMEDIATE Shift Out (0x0E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x0E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: ASL $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing ASL IMMEDIATE Negative FLag  (0x0E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x0E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x40;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: ASL $%04X

    Mem: 0x4469 : 0x80
    |}]
;;

let%expect_test "testing INC ABSOLUTE Add 1 (0xEE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xEE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x04469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: INC $%04X

    Mem: 0x4469 : 0x02
    |}]
;;

let%expect_test "testing INC ABSOLUTE Negative Flag (0xEE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xEE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x7F;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x04469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: INC $%04X

    Mem: 0x4469 : 0x80
    |}]
;;

let%expect_test "testing INC ABSOLUTE OverFlow (0xEE)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0xEE; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0xFF;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x04469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1003 inst: INC $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing LSR IMMEDIATE Basic (0x4E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x4E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x02;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: LSR $%04X

    Mem: 0x4469 : 0x01
    |}]
;;

let%expect_test "testing LSR IMMEDIATE Shift Into (0x4E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x4E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: LSR $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing LSR IMMEDIATE Hign Bit clear  (0x4E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x4E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: LSR $%04X

    Mem: 0x4469 : 0x40
    |}]
;;

let%expect_test "testing ROL ABSOLUTE Carry to bit 0 (0x2E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x2E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1003 inst: ROL $%04X

    Mem: 0x4469 : 0x01
    |}]
;;

let%expect_test "testing ROL ABSOLUTE Bit 7 to Carry (0x2E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x2E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x80;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: ROL $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing ROL ABSOLUTE Negative Flag (0x2E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x2E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x40;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: ROL $%04X

    Mem: 0x4469 : 0x80
    |}]
;;

let%expect_test "testing ROR ABSOLUTE Carry to Bit 7 (0x6E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x6E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x00;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1003 inst: ROR $%04X

    Mem: 0x4469 : 0x80
    |}]
;;

let%expect_test "testing ROR ABSOLUTE Bit 0 to Carry (0x6E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x6E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x01;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1003 inst: ROR $%04X

    Mem: 0x4469 : 0x00
    |}]
;;

let%expect_test "testing ROR ABSOLUTE Negative Flag (0x6E)" =
  (* 5 cycles , 2 byte *)
  (* z=0,n=0,c=0, 0b0000_0001(0x01) -> z=0,n=0,c=0, 0b0000_0010(0x02) *)
  let cycles = 6 in
  (* LDA $44 *)
  let pgm = [ 0x6E; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x4469) <- 0x7F;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x4469 last_computer.banks.(0x4469);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1003 inst: ROR $%04X

    Mem: 0x4469 : 0xBF
    |}]
;;

let%expect_test "testing JMP ABSOLUTE Normal (0x4C)" =
  let cycles = 3 in
  (* JMP $0x4469 *)
  let pgm = [ 0x4C; 0x69; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x4469 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x4469 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x4469 inst: JMP $%04X |}]
;;

let%expect_test "testing ASL ACCUMULATOR Basic (0x0A)" =
  let cycles = 2 in
  let pgm = [ 0x0A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: ASL A |}]
;;

let%expect_test "testing ASL ACCUMULATOR Shift Out (0x0A)" =
  let cycles = 2 in
  let pgm = [ 0x0A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1001 inst: ASL A |}]
;;

let%expect_test "testing ASL ACCUMULATOR Negative FLag  (0x0A)" =
  let cycles = 2 in
  let pgm = [ 0x0A ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x40;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x40; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: ASL A |}]
;;

let%expect_test "testing LSR ACCUMULATOR Basic (0x4A)" =
  let cycles = 2 in
  let pgm = [ 0x4A ] in
  let computer = init_test_computer pgm in
  computer.banks.(0x44) <- 0x02;
  let computer =
    { computer with cpu = { computer.cpu with a = 0x02; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: LSR A |}]
;;

let%expect_test "testing LSR ACCUMULATOR Shift Into (0x4A)" =
  let cycles = 2 in
  let pgm = [ 0x4A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1001 inst: LSR A |}]
;;

let%expect_test "testing LSR ACCUMULATOR Hign Bit clear  (0x4A)" =
  let cycles = 2 in
  let pgm = [ 0x4A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x40 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: LSR A |}]
;;

let%expect_test "testing ROR ACCUMULATOR Carry to Bit 7 (0x6A)" =
  let cycles = 2 in
  let pgm = [ 0x6A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: ROR A |}]
;;

let%expect_test "testing ROR ACCUMULATOR Bit 0 to Carry (0x6A)" =
  let cycles = 2 in
  let pgm = [ 0x6A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1001 inst: ROR A |}]
;;

let%expect_test "testing ROR ACCUMULATOR Negative Flag (0x6A)" =
  let cycles = 2 in
  let pgm = [ 0x6A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sr = 0x01 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0xBF x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1001 inst: ROR A |}]
;;

let%expect_test "testing ROL ACCUMULATOR Carry to bit 0 (0x2A)" =
  let cycles = 2 in
  let pgm = [ 0x2A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sr = 0x03 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: ROL A |}]
;;

let%expect_test "testing ROL ACCUMULATOR Bit 7 to Carry (0x2A)" =
  let cycles = 2 in
  let pgm = [ 0x2A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdiZC pc: 0x1001 inst: ROL A |}]
;;

let%expect_test "testing ROL ACCUMULATOR Negative Flag (0x2A)" =
  let cycles = 2 in
  let pgm = [ 0x2A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x40; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: ROL A |}]
;;

let%expect_test "testing INX IMPLIED Add 1 (0xE8)" =
  let cycles = 2 in
  let pgm = [ 0xE8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x01; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: INX |}]
;;

let%expect_test "testing INC IMPLIED Negative Flag (0xE8)" =
  let cycles = 2 in
  let pgm = [ 0xE8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x7F; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x80 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: INX |}]
;;

let%expect_test "testing INC IMPLIED OverFlow (0xE8)" =
  let cycles = 2 in
  let pgm = [ 0xE8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x00 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: INX |}]
;;

let%expect_test "testing INY IMPLIED Add 1 (0xC8)" =
  let cycles = 2 in
  let pgm = [ 0xC8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x01; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x02 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: INY |}]
;;

let%expect_test "testing INY IMPLIED Negative Flag (0xC8)" =
  let cycles = 2 in
  let pgm = [ 0xC8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x7F; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x80 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: INY |}]
;;

let%expect_test "testing INY IMPLIED OverFlow (0xC8)" =
  let cycles = 2 in
  let pgm = [ 0xC8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: INY |}]
;;

let%expect_test "testing DEX IMPLIED dec 1 (0xCA)" =
  let cycles = 2 in
  let pgm = [ 0xCA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x01 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: DEX |}]
;;

let%expect_test "testing DEX IMPLIED set Negative Flag (0xCA)" =
  let cycles = 2 in
  let pgm = [ 0xCA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x00; y = 0x03; sr = 0x02 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: DEX |}]
;;

let%expect_test "testing DEX IMPLIED Unset negative (0xCA)" =
  let cycles = 2 in
  let pgm = [ 0xCA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x80; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x7F y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: DEX |}]
;;

let%expect_test "testing DEX IMPLIED dec to zero (0xCA)" =
  let cycles = 2 in
  let pgm = [ 0xCA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x01; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x00 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: DEX |}]
;;

let%expect_test "testing DEY IMPLIED dec 1 (0x88)" =
  let cycles = 2 in
  let pgm = [ 0x88 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x01 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: DEY |}]
;;

let%expect_test "testing DEY IMPLIED set Negative Flag (0x88)" =
  let cycles = 2 in
  let pgm = [ 0x88 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x00; sr = 0x02 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: DEY |}]
;;

let%expect_test "testing DEY IMPLIED Unset negative (0x88)" =
  let cycles = 2 in
  let pgm = [ 0x88 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x80; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x7F sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: DEY |}]
;;

let%expect_test "testing DEY IMPLIED dec to zero (0x88)" =
  let cycles = 2 in
  let pgm = [ 0x88 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x01; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: DEY |}]
;;

let%expect_test "testing CLC IMPLIED standard (0x18)" =
  let cycles = 2 in
  let pgm = [ 0x18 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x81 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLC |}]
;;

let%expect_test "testing CLC IMPLIED No-Op (0x18)" =
  let cycles = 2 in
  let pgm = [ 0x18 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLC |}]
;;

let%expect_test "testing CLD IMPLIED standard (0xD8)" =
  let cycles = 2 in
  let pgm = [ 0xD8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x88 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLD |}]
;;

let%expect_test "testing CLD IMPLIED No-Op (0xD8)" =
  let cycles = 2 in
  let pgm = [ 0xD8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLD |}]
;;

let%expect_test "testing CLI IMPLIED standard (0x58)" =
  let cycles = 2 in
  let pgm = [ 0x58 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x84 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLI |}]
;;

let%expect_test "testing CLI IMPLIED No-Op (0x58)" =
  let cycles = 2 in
  let pgm = [ 0x58 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLI |}]
;;

let%expect_test "testing CLV IMPLIED standard (0xB8)" =
  let cycles = 2 in
  let pgm = [ 0xB8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x44 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: nv-bdIzc pc: 0x1001 inst: CLV |}]
;;

let%expect_test "testing CLV IMPLIED No-Op (0xB8)" =
  let cycles = 2 in
  let pgm = [ 0xB8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: CLV |}]
;;

let%expect_test "testing SEC IMPLIED standard (0x38)" =
  let cycles = 2 in
  let pgm = [ 0x38 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1001 inst: SEC |}]
;;

let%expect_test "testing SEC IMPLIED No-Op (0x38)" =
  let cycles = 2 in
  let pgm = [ 0x38 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x81 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdizC pc: 0x1001 inst: SEC |}]
;;

let%expect_test "testing SED IMPLIED standard (0xF8)" =
  let cycles = 2 in
  let pgm = [ 0xF8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bDizc pc: 0x1001 inst: SED |}]
;;

let%expect_test "testing SED IMPLIED No-Op (0xF8)" =
  let cycles = 2 in
  let pgm = [ 0xF8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x88 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bDizc pc: 0x1001 inst: SED |}]
;;

let%expect_test "testing SEI IMPLIED standard (0x78)" =
  let cycles = 2 in
  let pgm = [ 0x78 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdIzc pc: 0x1001 inst: SEI |}]
;;

let%expect_test "testing SEI IMPLIED No-Op (0x78)" =
  let cycles = 2 in
  let pgm = [ 0x78 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sr = 0x84 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFF sr: Nv-bdIzc pc: 0x1001 inst: SEI |}]
;;

let%expect_test "testing TAX IMPLIED Positive (0xAA)" =
  let cycles = 2 in
  let pgm = [ 0xAA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x42 x: 0x42 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: TAX |}]
;;

let%expect_test "testing TAX IMPLIED Negative (0xAA)" =
  let cycles = 2 in
  let pgm = [ 0xAA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x80 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: TAX |}]
;;

let%expect_test "testing TAX IMPLIED Zero flag (0xAA)" =
  let cycles = 2 in
  let pgm = [ 0xAA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x00 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: TAX |}]
;;

let%expect_test "testing TXA IMPLIED Positive (0x8A)" =
  let cycles = 2 in
  let pgm = [ 0x8A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x42; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x42 x: 0x42 y: 0x03 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: TXA |}]
;;

let%expect_test "testing TXA IMPLIED Negative (0x8A)" =
  let cycles = 2 in
  let pgm = [ 0x8A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x80; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x80 y: 0x03 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: TXA |}]
;;

let%expect_test "testing TXA IMPLIED Zero flag (0x8A)" =
  let cycles = 2 in
  let pgm = [ 0x8A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x00; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x00 y: 0x03 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: TXA |}]
;;

let%expect_test "testing TAY IMPLIED Positive (0xA8)" =
  let cycles = 2 in
  let pgm = [ 0xA8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x42 x: 0x02 y: 0x42 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: TAY |}]
;;

let%expect_test "testing TAY IMPLIED Negative (0xA8)" =
  let cycles = 2 in
  let pgm = [ 0xA8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x80 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: TAY |}]
;;

let%expect_test "testing TAY IMPLIED Zero flag (0xA8)" =
  let cycles = 2 in
  let pgm = [ 0xA8 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: TAY |}]
;;

let%expect_test "testing TYA IMPLIED Positive (0x98)" =
  let cycles = 2 in
  let pgm = [ 0x98 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x42; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x42 x: 0x02 y: 0x42 sp: 0xFF sr: nv-bdizc pc: 0x1001 inst: TYA |}]
;;

let%expect_test "testing TYA IMPLIED Negative (0x98)" =
  let cycles = 2 in
  let pgm = [ 0x98 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x80; sr = 0x00 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x80 sp: 0xFF sr: Nv-bdizc pc: 0x1001 inst: TYA |}]
;;

let%expect_test "testing TYA IMPLIED Zero flag (0x98)" =
  let cycles = 2 in
  let pgm = [ 0x98 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x00; sr = 0x80 } }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x00 sp: 0xFF sr: nv-bdiZc pc: 0x1001 inst: TYA |}]
;;

let%expect_test "testing TXS IMPLIED Positive (0x9A)" =
  let cycles = 2 in
  let pgm = [ 0x9A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x42; y = 0x03; sp = 0xFF; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x42 y: 0x03 sp: 0x42 sr: nv-bdizc pc: 0x1001 inst: TXS |}]
;;

let%expect_test "testing TXS IMPLIED Negative (0x9A)" =
  let cycles = 2 in
  let pgm = [ 0x9A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x80; y = 0x03; sp = 0xFF; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x80 y: 0x03 sp: 0x80 sr: Nv-bdizc pc: 0x1001 inst: TXS |}]
;;

let%expect_test "testing TXS IMPLIED Zero flag (0x9A)" =
  let cycles = 2 in
  let pgm = [ 0x9A ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x00; y = 0x03; sp = 0xFF; sr = 0x80 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x00 y: 0x03 sp: 0x00 sr: nv-bdiZc pc: 0x1001 inst: TXS |}]
;;

let%expect_test "testing TSX IMPLIED Positive (0xBA)" =
  let cycles = 2 in
  let pgm = [ 0xBA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0x42; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x42 y: 0x03 sp: 0x42 sr: nv-bdizc pc: 0x1001 inst: TSX |}]
;;

let%expect_test "testing TSX IMPLIED Negative (0xBA)" =
  let cycles = 2 in
  let pgm = [ 0xBA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0x80; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x80 y: 0x03 sp: 0x80 sr: Nv-bdizc pc: 0x1001 inst: TSX |}]
;;

let%expect_test "testing TSX IMPLIED Zero flag (0xBA)" =
  let cycles = 2 in
  let pgm = [ 0xBA ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0x00; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x00 y: 0x03 sp: 0x00 sr: nv-bdiZc pc: 0x1001 inst: TSX |}]
;;

let%expect_test "testing PHA IMPLIED (0x48)" =
  let cycles = 3 in
  let pgm = [ 0x48 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFE; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FC last_computer.banks.(0x1FC);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FD last_computer.banks.(0x1FD);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FE last_computer.banks.(0x1FE);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FF last_computer.banks.(0x1FF);
  [%expect
    {|
    ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1001 inst: PHA

    Mem: 0x01FC : 0xFF
    Mem: 0x01FD : 0xFF
    Mem: 0x01FE : 0x01
    Mem: 0x01FF : 0xFF
    |}]
;;

let%expect_test "testing PHP IMPLIED (0x08)" =
  let cycles = 3 in
  let pgm = [ 0x08 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFE; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FC last_computer.banks.(0x1FC);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FD last_computer.banks.(0x1FD);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FE last_computer.banks.(0x1FE);
  printf "Mem: 0x%04X : 0x%02X\n" 0x1FF last_computer.banks.(0x1FF);
  [%expect
    {|
    ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1001 inst: PHP

    Mem: 0x01FC : 0xFF
    Mem: 0x01FD : 0xFF
    Mem: 0x01FE : 0x00
    Mem: 0x01FF : 0xFF
    |}]
;;

let%expect_test "testing PLA IMPLIED Positive (0x68)" =
  let cycles = 4 in
  let pgm = [ 0x68 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x1FC) <- 0x0C;
  computer.banks.(0x1FD) <- 0x10;
  computer.banks.(0x1FE) <- 0x1E;
  computer.banks.(0x1FF) <- 0x1F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x10 x: 0x02 y: 0x03 sp: 0xFE sr: nv-bdizc pc: 0x1001 inst: PLA |}]
;;

let%expect_test "testing PLA IMPLIED Negative (0x68)" =
  let cycles = 4 in
  let pgm = [ 0x68 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x1FC) <- 0x0C;
  computer.banks.(0x1FD) <- 0x80;
  computer.banks.(0x1FE) <- 0x1E;
  computer.banks.(0x1FF) <- 0x1F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFE sr: Nv-bdizc pc: 0x1001 inst: PLA |}]
;;

let%expect_test "testing PLA IMPLIED Zero (0x68)" =
  let cycles = 4 in
  let pgm = [ 0x68 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x1FC) <- 0x0C;
  computer.banks.(0x1FD) <- 0x00;
  computer.banks.(0x1FE) <- 0x1E;
  computer.banks.(0x1FF) <- 0x1F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFE sr: nv-bdiZc pc: 0x1001 inst: PLA |}]
;;

let%expect_test "testing PLP IMPLIED (0x28)" =
  let cycles = 4 in
  let pgm = [ 0x28 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x1FC) <- 0x0C;
  computer.banks.(0x1FD) <- 0b1100_0011;
  computer.banks.(0x1FE) <- 0x1E;
  computer.banks.(0x1FF) <- 0x1F;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFE sr: NV-bdiZC pc: 0x1001 inst: PLP |}]
;;

let%expect_test "testing LDA ZEROPAGEX (0xB5) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xB5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x04 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDA $%02X,X |}]
;;

let%expect_test "testing LDA ZEROPAGEX (0xB5) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xB5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: LDA $%02X,X |}]
;;

let%expect_test "testing LDA ZEROPAGEX (0xB5) zero " =
  let cycles = 4 in
  let pgm = [ 0xB5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: LDA $%02X,X |}]
;;

let%expect_test "testing LDA ZEROPAGEX (0xB5) wrap around" =
  let cycles = 4 in
  let pgm = [ 0xB5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x04 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDA $%02X,X |}]
;;

let%expect_test "testing LDY ZEROPAGEX (0xB4) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xB4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x04 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDY $%02X,X |}]
;;

let%expect_test "testing LDY ZEROPAGEX (0xB4) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xB4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x84 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: LDY $%02X,X |}]
;;

let%expect_test "testing LDY ZEROPAGEX (0xB4) zero " =
  let cycles = 4 in
  let pgm = [ 0xB4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x00 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: LDY $%02X,X |}]
;;

let%expect_test "testing LDY ZEROPAGEX (0xB4) wrap around" =
  let cycles = 4 in
  let pgm = [ 0xB4; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x04 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDY $%02X,X |}]
;;

let%expect_test "testing EOR ZEROPAGEX (0x55) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x55; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: EOR $%02X,X |}]
;;

let%expect_test "testing EOR ZEROPAGEX (0x55) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x55; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x85 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: EOR $%02X,X |}]
;;

let%expect_test "testing EOR ZEROPAGEX (0x55) zero " =
  let cycles = 4 in
  let pgm = [ 0x55; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: EOR $%02X,X |}]
;;

let%expect_test "testing EOR ZEROPAGEX (0x55) wrap around" =
  let cycles = 4 in
  let pgm = [ 0x55; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: EOR $%02X,X |}]
;;

let%expect_test "testing AND ZEROPAGEX (0x35) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x35; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: AND $%02X,X |}]
;;

let%expect_test "testing AND ZEROPAGEX (0x35) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x35; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x81; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: AND $%02X,X |}]
;;

let%expect_test "testing AND ZEROPAGEX (0x35) zero " =
  let cycles = 4 in
  let pgm = [ 0x35; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: AND $%02X,X |}]
;;

let%expect_test "testing AND ZEROPAGEX (0x35) wrap around" =
  let cycles = 4 in
  let pgm = [ 0x35; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: AND $%02X,X |}]
;;

let%expect_test "testing ORA ZEROPAGEX (0x15) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x15; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ORA $%02X,X |}]
;;

let%expect_test "testing ORA ZEROPAGEX (0x15) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x15; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ORA $%02X,X |}]
;;

let%expect_test "testing ORA ZEROPAGEX (0x15) zero " =
  let cycles = 4 in
  let pgm = [ 0x15; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: ORA $%02X,X |}]
;;

let%expect_test "testing ORA ZEROPAGEX (0x15) wrap around" =
  let cycles = 4 in
  let pgm = [ 0x15; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ORA $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x06 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) Incoming carry" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x46) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x07 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) Genrating Carry " =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) Pos+Pos=Neg" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: NV-bdizc pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) Pos+Neg" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) Neg+Neg=Pos" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing ADC ZEROPAGEX (0x75) wrap around" =
  let cycles = 4 in
  let pgm = [ 0x75; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x06 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ADC $%02X,X |}]
;;

let%expect_test "testing STA ZEROPAGEX (0x95)" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x95; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: STA $%02X,X

    Mem: 0x0046 : 0x01
    |}]
;;

let%expect_test "testing STA ZEROPAGEX wrap around (0x95)" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x95; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: STA $%02X,X

    Mem: 0x0043 : 0x01
    |}]
;;

let%expect_test "testing STY ZEROPAGEX (0x94)" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x94; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: STY $%02X,X

    Mem: 0x0046 : 0x03
    |}]
;;

let%expect_test "testing STY ZEROPAGEX wrap around (0x94)" =
  (* 2 cycles , 1 byte *)
  let cycles = 4 in
  (* LDA $44 *)
  let pgm = [ 0x94; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: STY $%02X,X

    Mem: 0x0043 : 0x03
    |}]
;;

let%expect_test "testing ASL ZEROPAGEX (0x16) Basic" =
  let cycles = 6 in
  let pgm = [ 0x16; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ASL $%02X,X

    Mem: 0x0046 : 0x04
    |}]
;;

let%expect_test "testing ASL ZEROPAGEX (0x16) Shift out" =
  let cycles = 6 in
  let pgm = [ 0x16; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: ASL $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing ASL ZEROPAGEX (0x16) Negative Flag" =
  let cycles = 6 in
  let pgm = [ 0x16; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x40;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ASL $%02X,X

    Mem: 0x0046 : 0x80
    |}]
;;

let%expect_test "testing ASL ZEROPAGEX (0x16) Wrap around" =
  let cycles = 6 in
  let pgm = [ 0x16; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ASL $%02X,X

    Mem: 0x0043 : 0x04
    |}]
;;

let%expect_test "testing LSR ZEROPAGEX (0x56) Basic" =
  let cycles = 6 in
  let pgm = [ 0x56; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LSR $%02X,X

    Mem: 0x0046 : 0x01
    |}]
;;

let%expect_test "testing LSR ZEROPAGEX (0x56) Shift out" =
  let cycles = 6 in
  let pgm = [ 0x56; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: LSR $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing LSR ZEROPAGEX (0x56) High bit clear" =
  let cycles = 6 in
  let pgm = [ 0x56; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x46) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LSR $%02X,X

    Mem: 0x0046 : 0x40
    |}]
;;

let%expect_test "testing LSR ZEROPAGEX (0x56) Wrap around" =
  let cycles = 6 in
  let pgm = [ 0x56; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LSR $%02X,X

    Mem: 0x0043 : 0x01
    |}]
;;

let%expect_test "testing ROL ZEROPAGEX (0x36) Carry to bit 0" =
  let cycles = 6 in
  let pgm = [ 0x36; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ROL $%02X,X

    Mem: 0x0046 : 0x01
    |}]
;;

let%expect_test "testing ROL ZEROPAGEX (0x36) bit 7 to carry" =
  let cycles = 6 in
  let pgm = [ 0x36; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x80 }
    }
  in
  computer.banks.(0x46) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: ROL $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing ROL ZEROPAGEX (0x36) Negative Flag" =
  let cycles = 6 in
  let pgm = [ 0x36; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x40;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ROL $%02X,X

    Mem: 0x0046 : 0x80
    |}]
;;

let%expect_test "testing ROL ZEROPAGEX (0x36) Wrap Around" =
  let cycles = 6 in
  let pgm = [ 0x36; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x43) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: ROL $%02X,X

    Mem: 0x0043 : 0x01
    |}]
;;

let%expect_test "testing ROR ZEROPAGEX (0x76) carry to bit 7" =
  let cycles = 6 in
  let pgm = [ 0x76; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ROR $%02X,X

    Mem: 0x0046 : 0x80
    |}]
;;

let%expect_test "testing ROR ZEROPAGEX (0x76) bit 0 to carry" =
  let cycles = 6 in
  let pgm = [ 0x76; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: ROR $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing ROR ZEROPAGEX (0x76) Negative Flag" =
  let cycles = 6 in
  let pgm = [ 0x76; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x46) <- 0x7F;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizC pc: 0x1002 inst: ROR $%02X,X

    Mem: 0x0046 : 0xBF
    |}]
;;

let%expect_test "testing ROR ZEROPAGEX (0x76) Wrap Around" =
  let cycles = 6 in
  let pgm = [ 0x76; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x43) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: ROR $%02X,X

    Mem: 0x0043 : 0x80
    |}]
;;

let%expect_test "testing CMP ZEROPAGEX (0xD5) Equality " =
  let cycles = 4 in
  let pgm = [ 0xD5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x42 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: CMP $%02X,X |}]
;;

let%expect_test "testing CMP ZEROPAGEX (0xD5) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xD5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0xFF; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizC pc: 0x1002 inst: CMP $%02X,X |}]
;;

let%expect_test "testing CMP ZEROPAGEX (0xD5) Less than" =
  let cycles = 4 in
  let pgm = [ 0xD5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x02; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: CMP $%02X,X |}]
;;

let%expect_test "testing CMP ZEROPAGEX (0xD5) wrap around" =
  let cycles = 4 in
  let pgm = [ 0xD5; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x42 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1002 inst: CMP $%02X,X |}]
;;

let%expect_test "testing DEC ZEROPAGEX (0xD6) Dec 1" =
  let cycles = 6 in
  let pgm = [ 0xD6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: DEC $%02X,X

    Mem: 0x0046 : 0x01
    |}]
;;

let%expect_test "testing DEC ZEROPAGEX (0xD6) Set negative flag" =
  let cycles = 6 in
  let pgm = [ 0xD6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: DEC $%02X,X

    Mem: 0x0046 : 0xFF
    |}]
;;

let%expect_test "testing DEC ZEROPAGEX (0xD6) Unset negative flag" =
  let cycles = 6 in
  let pgm = [ 0xD6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: DEC $%02X,X

    Mem: 0x0046 : 0x7F
    |}]
;;

let%expect_test "testing DEC ZEROPAGEX (0xD6) Dec to zero" =
  let cycles = 6 in
  let pgm = [ 0xD6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: DEC $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing DEC ZEROPAGEX (0xD6) Wrap around" =
  let cycles = 6 in
  let pgm = [ 0xD6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: DEC $%02X,X

    Mem: 0x0043 : 0x01
    |}]
;;

let%expect_test "testing INC ZEROPAGEX (0xF6) Inc 1" =
  let cycles = 6 in
  let pgm = [ 0xF6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: INC $%02X,X

    Mem: 0x0046 : 0x03
    |}]
;;

let%expect_test "testing INC ZEROPAGEX (0xF6) Set negative flag" =
  let cycles = 6 in
  let pgm = [ 0xF6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x7F;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: INC $%02X,X

    Mem: 0x0046 : 0x80
    |}]
;;

let%expect_test "testing INC ZEROPAGEX (0xF6) Overflow" =
  let cycles = 6 in
  let pgm = [ 0xF6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0xFF;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x46 last_computer.banks.(0x46);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: INC $%02X,X

    Mem: 0x0046 : 0x00
    |}]
;;

let%expect_test "testing INC ZEROPAGEX (0xF6) Wrap around" =
  let cycles = 6 in
  let pgm = [ 0xF6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x43 last_computer.banks.(0x43);
  [%expect
    {|
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: INC $%02X,X

    Mem: 0x0043 : 0x03
    |}]
;;

let%expect_test "testing LDX ZEROPAGEY (0xB6) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xB6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x04 y: 0x02 sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDX $%02X,Y |}]
;;

let%expect_test "testing LDX ZEROPAGEY (0xB6) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xB6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x84 y: 0x02 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: LDX $%02X,Y |}]
;;

let%expect_test "testing LDX ZEROPAGEY (0xB6) zero " =
  let cycles = 4 in
  let pgm = [ 0xB6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x02; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x46) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x02 sp: 0xFD sr: nv-bdiZc pc: 0x1002 inst: LDX $%02X,Y |}]
;;

let%expect_test "testing LDX ZEROPAGEY (0xB6) wrap around" =
  let cycles = 4 in
  let pgm = [ 0xB6; 0x44 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  (*
  0x44 <- 0x50
  x register = 0xFF
  LDA $0x44,X will read 0x50 and add 0xFF => 0x4F
  and a register should be the value ad 0x4F
*)
  computer.banks.(0x43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x04 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1002 inst: LDX $%02X,Y |}]
;;

let%expect_test "testing LDA ABSOLUTEX (0xBD) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xBD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x04 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDA $%04X,X |}]
;;

let%expect_test "testing LDA ABSOLUTEX (0xBD) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xBD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: LDA $%04X,X |}]
;;

let%expect_test "testing LDA ABSOLUTEX (0xBD) zero" =
  let cycles = 4 in
  let pgm = [ 0xBD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: LDA $%04X,X |}]
;;

let%expect_test "testing LDA ABSOLUTEX (0xBD) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xBD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x04 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDA $%04X,X |}]
;;

let%expect_test "testing LDY ABSOLUTEX (0xBC) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xBC; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x04 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDY $%04X,X |}]
;;

let%expect_test "testing LDY ABSOLUTEX (0xBC) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xBC; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x84 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: LDY $%04X,X |}]
;;

let%expect_test "testing LDY ZEROPAGEX (0xBC) zero " =
  let cycles = 4 in
  let pgm = [ 0xBC; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x00 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: LDY $%04X,X |}]
;;

let%expect_test "testing LDY ABSOLUTE (0xBC) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xBc; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x04 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDY $%04X,X |}]
;;

let%expect_test "testing EOR ABSOLUTEX (0x5D) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x5D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: EOR $%04X,X |}]
;;

let%expect_test "testing EOR ABSOLUTEX (0x5D) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x5D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x85 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: EOR $%04X,X |}]
;;

let%expect_test "testing EOR ABSOLUTEX (0x5D) zero " =
  let cycles = 4 in
  let pgm = [ 0x5D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: EOR $%04X,X |}]
;;

let%expect_test "testing EOR ABSOLUTEX (0x5D) wrap around" =
  let cycles = 5 in
  let pgm = [ 0x5D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: EOR $%04X,X |}]
;;

let%expect_test "testing AND ABSOLUTEX (0x3D) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x3D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: AND $%04X,X |}]
;;

let%expect_test "testing AND ABSOLUTEX (0x3D) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x3D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x81; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: AND $%04X,X |}]
;;

let%expect_test "testing AND ABSOLUTEX (0x3D) zero " =
  let cycles = 4 in
  let pgm = [ 0x3D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: AND $%04X,X |}]
;;

let%expect_test "testing AND ABSOLUTEX (0x3D) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x3D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6943) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: AND $%04X,X |}]
;;

let%expect_test "testing ORA ABSOLUTEX (0x1D) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x1D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ORA $%04X,X |}]
;;

let%expect_test "testing ORA ABSOLUTEX (0x1D) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x1D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ORA $%04X,X |}]
;;

let%expect_test "testing ORA ABSOLUTEX (0x1D) zero " =
  let cycles = 4 in
  let pgm = [ 0x1D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: ORA $%04X,X |}]
;;

let%expect_test "testing ORA ABSOLUTEX (0x1D) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x1D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ORA $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x06 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Incoming carry" =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x07 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Genrating Carry " =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Pos+Pos=Neg" =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: NV-bdizc pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Pos+Neg" =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Neg+Neg=Pos" =
  let cycles = 4 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing ADC ABSOLUTEX (0x7D) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x7D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x06 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,X |}]
;;

let%expect_test "testing STA ABSOLUTEX (0x9D)" =
  (* 2 cycles , 1 byte *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x9D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: STA $%04X,X

    Mem: 0x6946 : 0x01
    |}]
;;

let%expect_test "testing STA ABSOLUTEX (0x9D) Cross PAge" =
  (* 2 cycles , 1 byte *)
  let cycles = 5 in
  (* LDA $44 *)
  let pgm = [ 0x9D; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: STA $%04X,X

    Mem: 0x6A43 : 0x01
    |}]
;;

let%expect_test "testing ASL ABSOLUTEX (0x1E) Basic" =
  let cycles = 7 in
  let pgm = [ 0x1E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ASL $%04X,X

    Mem: 0x6946 : 0x04
    |}]
;;

let%expect_test "testing ASL ABSOLUTEX (0x1E) Shift out" =
  let cycles = 7 in
  let pgm = [ 0x1E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: ASL $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing ASL ABSOLUTEX (0x1E) Negative Flag" =
  let cycles = 7 in
  let pgm = [ 0x1E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x40;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ASL $%04X,X

    Mem: 0x6946 : 0x80
    |}]
;;

let%expect_test "testing ASL ABSOLUTEX (0x1E) Cross Page" =
  let cycles = 7 in
  let pgm = [ 0x1E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ASL $%04X,X

    Mem: 0x6A43 : 0x04
    |}]
;;

let%expect_test "testing CMP ABSOLUTEX (0xDD) Equality " =
  let cycles = 4 in
  let pgm = [ 0xDD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x42 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: CMP $%04X,X |}]
;;

let%expect_test "testing CMP ABSOLUTEX (0xDD) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xDD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0xFF; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizC pc: 0x1003 inst: CMP $%04X,X |}]
;;

let%expect_test "testing CMP ABSOLUTEX (0xDD) Less than" =
  let cycles = 4 in
  let pgm = [ 0xDD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x02; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: CMP $%04X,X |}]
;;

let%expect_test "testing CMP ABSOLUTEX (0xDD) PAge Cross" =
  let cycles = 5 in
  let pgm = [ 0xDD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x42 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: CMP $%04X,X |}]
;;

let%expect_test "testing DEC ABSOLUTEX (0xDE) Dec 1" =
  let cycles = 7 in
  let pgm = [ 0xDE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: DEC $%04X,X

    Mem: 0x6946 : 0x01
    |}]
;;

let%expect_test "testing DEC ABSOLUTEX (0xDE) Set negative flag" =
  let cycles = 7 in
  let pgm = [ 0xDE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: DEC $%04X,X

    Mem: 0x6946 : 0xFF
    |}]
;;

let%expect_test "testing DEC ABSOLUTEX (0xDE) Unset negative flag" =
  let cycles = 7 in
  let pgm = [ 0xDE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: DEC $%04X,X

    Mem: 0x6946 : 0x7F
    |}]
;;

let%expect_test "testing DEC ABSOLUTEX (0xDE) Dec to zero" =
  let cycles = 7 in
  let pgm = [ 0xDE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: DEC $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing DEC ABSOLUTEX (0xDE) Page Cross" =
  let cycles = 7 in
  let pgm = [ 0xDE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: DEC $%04X,X

    Mem: 0x6A43 : 0x01
    |}]
;;

let%expect_test "testing INC ABSOLUTEX (0xFE) Inc 1" =
  let cycles = 7 in
  let pgm = [ 0xFE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: INC $%04X,X

    Mem: 0x6946 : 0x03
    |}]
;;

let%expect_test "testing INC ABSOLUTEX (0xFE) Set negative flag" =
  let cycles = 7 in
  let pgm = [ 0xFE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x7F;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: INC $%04X,X

    Mem: 0x6946 : 0x80
    |}]
;;

let%expect_test "testing INC ABSOLUTEX (0xFE) Overflow" =
  let cycles = 7 in
  let pgm = [ 0xFE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0xFF;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: INC $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing INC ABSOLUTEX (0xFE) Page Cross" =
  let cycles = 7 in
  let pgm = [ 0xFE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: INC $%04X,X

    Mem: 0x6A43 : 0x03
    |}]
;;

let%expect_test "testing ROR ABSOLUTEX (0x7E) carry to bit 7" =
  let cycles = 7 in
  let pgm = [ 0x7E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ROR $%04X,X

    Mem: 0x6946 : 0x80
    |}]
;;

let%expect_test "testing ROR ABSOLUTEX (0x7E) bit 0 to carry" =
  let cycles = 7 in
  let pgm = [ 0x7E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: ROR $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing ROR ABSOLUTEX (0x7E) Negative Flag" =
  let cycles = 7 in
  let pgm = [ 0x7E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x7F;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizC pc: 0x1003 inst: ROR $%04X,X

    Mem: 0x6946 : 0xBF
    |}]
;;

let%expect_test "testing ROR ABSOLUTEX (0x7E) Page Cross" =
  let cycles = 7 in
  let pgm = [ 0x7E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x6A43) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ROR $%04X,X

    Mem: 0x6A43 : 0x80
    |}]
;;

let%expect_test "testing ROL ABSOLUTEX (0x3E) Carry to bit 0" =
  let cycles = 7 in
  let pgm = [ 0x3E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x6946) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ROL $%04X,X

    Mem: 0x6946 : 0x01
    |}]
;;

let%expect_test "testing ROL ABSOLUTEX (0x3E) bit 7 to carry" =
  let cycles = 7 in
  let pgm = [ 0x3E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x80 }
    }
  in
  computer.banks.(0x6946) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: ROL $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing ROL ABSOLUTEX (0x3E) Negative Flag" =
  let cycles = 7 in
  let pgm = [ 0x3E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x40;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ROL $%04X,X

    Mem: 0x6946 : 0x80
    |}]
;;

let%expect_test "testing ROL ABSOLUTEX (0x3E) Wrap Around" =
  let cycles = 7 in
  let pgm = [ 0x3E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x03 }
    }
  in
  computer.banks.(0x6A43) <- 0x00;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ROL $%04X,X

    Mem: 0x6A43 : 0x01
    |}]
;;

let%expect_test "testing LSR ABSOLUTEX (0x5E) Basic" =
  let cycles = 7 in
  let pgm = [ 0x5E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LSR $%04X,X

    Mem: 0x6946 : 0x01
    |}]
;;

let%expect_test "testing LSR ABSOLUTEX (0x5E) Shift out" =
  let cycles = 7 in
  let pgm = [ 0x5E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: LSR $%04X,X

    Mem: 0x6946 : 0x00
    |}]
;;

let%expect_test "testing LSR ABSOLUTEX (0x5E) High bit clear" =
  let cycles = 7 in
  let pgm = [ 0x5E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x80;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6946 last_computer.banks.(0x6946);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LSR $%04X,X

    Mem: 0x6946 : 0x40
    |}]
;;

let%expect_test "testing LSR ABSOLUTEX (0x5E) Page Cross" =
  let cycles = 7 in
  let pgm = [ 0x5E; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LSR $%04X,X

    Mem: 0x6A43 : 0x01
    |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x06 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Incoming carry" =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6947) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x07 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Genrating Carry " =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Pos+Pos=Neg" =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: NV-bdizc pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Pos+Neg" =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x7F; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x80;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Neg+Neg=Pos" =
  let cycles = 4 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0xFF;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing ADC ABSOLUTEY (0x79) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x79; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x06 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ADC $%04X,Y |}]
;;

let%expect_test "testing STA ABSOLUTEY (0x99)" =
  let cycles = 5 in
  let pgm = [ 0x99; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6947 last_computer.banks.(0x6947);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: STA $%04X,Y

    Mem: 0x6947 : 0x01
    |}]
;;

let%expect_test "testing STA ABSOLUTEY (0x99) Cross PAge" =
  let cycles = 5 in
  let pgm = [ 0x99; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0xEA;
  let executions = execute_cycles cycles computer in
  let last_computer = List.last_exn executions in
  dump_last_execution executions;
  printf "Mem: 0x%04X : 0x%02X" 0x6A43 last_computer.banks.(0x6A43);
  [%expect
    {|
    ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: STA $%04X,Y

    Mem: 0x6A43 : 0x01
    |}]
;;

let%expect_test "testing ORA ABSOLUTEY (0x19) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x19; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ORA $%04X,Y |}]
;;

let%expect_test "testing ORA ABSOLUTEY (0x19) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x19; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: ORA $%04X,Y |}]
;;

let%expect_test "testing ORA ABSOLUTEY (0x19) zero " =
  let cycles = 4 in
  let pgm = [ 0x19; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x00; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: ORA $%04X,Y |}]
;;

let%expect_test "testing ORA ABSOLUTEY (0x19) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x19; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: ORA $%04X,Y |}]
;;

let%expect_test "testing AND ABSOLUTEY (0x39) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x39; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: AND $%04X,Y |}]
;;

let%expect_test "testing AND ABSOLUTEY (0x39) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x39; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x81; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x80 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: AND $%04X,Y |}]
;;

let%expect_test "testing AND ABSOLUTEY (0x39) zero " =
  let cycles = 4 in
  let pgm = [ 0x39; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: AND $%04X,Y |}]
;;

let%expect_test "testing AND ABSOLUTEY (0x39) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x39; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x05;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: AND $%04X,Y |}]
;;

let%expect_test "testing EOR ABSOLUTEY (0x59) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0x59; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: EOR $%04X,Y |}]
;;

let%expect_test "testing EOR ABSOLUTEY (0x59) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0x59; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x85 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: EOR $%04X,Y |}]
;;

let%expect_test "testing EOR ABSOLUTEY (0x59) zero " =
  let cycles = 4 in
  let pgm = [ 0x59; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: EOR $%04X,Y |}]
;;

let%expect_test "testing EOR ABSOLUTEY (0x59) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0x59; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x05 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: EOR $%04X,Y |}]
;;

let%expect_test "testing CMP ABSOLUTEY (0xD9) Equality " =
  let cycles = 4 in
  let pgm = [ 0xD9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x42 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: CMP $%04X,Y |}]
;;

let%expect_test "testing CMP ABSOLUTEY (0xD9) Greater than" =
  let cycles = 4 in
  let pgm = [ 0xD9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0xFF; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizC pc: 0x1003 inst: CMP $%04X,Y |}]
;;

let%expect_test "testing CMP ABSOLUTEY (0xD9) Less than" =
  let cycles = 4 in
  let pgm = [ 0xD9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x02; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x03;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: CMP $%04X,Y |}]
;;

let%expect_test "testing CMP ABSOLUTEY (0xD9) PAge Cross" =
  let cycles = 5 in
  let pgm = [ 0xD9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x42; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x42;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x42 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdiZC pc: 0x1003 inst: CMP $%04X,Y |}]
;;

let%expect_test "testing LDA ABSOLUTEX (0xB9) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xB9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x04 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDA $%04X,Y |}]
;;

let%expect_test "testing LDA ABSOLITEX (0xB9) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xB9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x84 x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: LDA $%04X,Y |}]
;;

let%expect_test "testing LDA ABSOLITEX (0xB9) zero" =
  let cycles = 4 in
  let pgm = [ 0xB9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x00 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: LDA $%04X,Y |}]
;;

let%expect_test "testing LDA ABSOLITEX (0xB9) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xB9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x04 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDA $%04X,Y |}]
;;

let%expect_test "testing LDX ABSOLUTEY (0xBE) non-zero positive" =
  let cycles = 4 in
  let pgm = [ 0xBE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x04 y: 0x03 sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDX $%04X,Y |}]
;;

let%expect_test "testing LDX ABSOLUTEY (0xBE) non-zero negative" =
  let cycles = 4 in
  let pgm = [ 0xBE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x84;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x84 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: LDX $%04X,Y |}]
;;

let%expect_test "testing LDX ABSOLUTEY (0xBE) zero " =
  let cycles = 4 in
  let pgm = [ 0xBE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x00;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x00 y: 0x03 sp: 0xFD sr: nv-bdiZc pc: 0x1003 inst: LDX $%04X,Y |}]
;;

let%expect_test "testing LDX ABSOLUTEY (0xBE) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xBE; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6A43) <- 0x04;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x01 x: 0x04 y: 0xFF sp: 0xFD sr: nv-bdizc pc: 0x1003 inst: LDX $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTEY (0xF9) No Borrow" =
  let cycles = 4 in
  let pgm = [ 0xF9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6947) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTEY (0xF9) with borrow" =
  let cycles = 4 in
  let pgm = [ 0xF9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6947) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTEY (0xF9) Underflow" =
  let cycles = 4 in
  let pgm = [ 0xF9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6947) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: SBC $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTEY (0xF9) Overflow" =
  let cycles = 4 in
  let pgm = [ 0xF9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6947) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1003 inst: SBC $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTEY (0xF9) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xF9; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0xFF; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x02 y: 0xFF sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,Y |}]
;;

let%expect_test "testing SBC ABSOLUTE (0xED) No Borrow" =
  let cycles = 4 in
  let pgm = [ 0xED; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6944) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X |}]
;;

let%expect_test "testing SBC ABSOLUTE (0xED) with borrow" =
  let cycles = 4 in
  let pgm = [ 0xED; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6944) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X |}]
;;

let%expect_test "testing SBC ABSOLUTE (0xED) Underflow" =
  let cycles = 4 in
  let pgm = [ 0xED; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6944) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: SBC $%04X |}]
;;

let%expect_test "testing SBC ABSOLUTE (0xED) Overflow" =
  let cycles = 4 in
  let pgm = [ 0xED; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6944) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1003 inst: SBC $%04X |}]
;;

let%expect_test "testing SBC ABSOLUTEX (0xFD) No Borrow" =
  let cycles = 4 in
  let pgm = [ 0xFD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,X |}]
;;

let%expect_test "testing SBC ABSOLUTEX (0xFD) with borrow" =
  let cycles = 4 in
  let pgm = [ 0xFD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,X |}]
;;

let%expect_test "testing SBC ABSOLUTEX (0xFD) Underflow" =
  let cycles = 4 in
  let pgm = [ 0xFD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1003 inst: SBC $%04X,X |}]
;;

let%expect_test "testing SBC ABSOLUTEX (0xFD) Overflow" =
  let cycles = 4 in
  let pgm = [ 0xFD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6946) <- 0x01;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1003 inst: SBC $%04X,X |}]
;;

let%expect_test "testing SBC ABSOLUTEX (0xFD) Page Cross" =
  let cycles = 5 in
  let pgm = [ 0xFD; 0x44; 0x69 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0xFF; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  computer.banks.(0x6A43) <- 0x02;
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1003 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1003 data: 0xFF a: 0x03 x: 0xFF y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1003 inst: SBC $%04X,X |}]
;;

let%expect_test "testing SBC Binary IMMEDIATE (0xE9) No Borrow" =
  let cycles = 2 in
  let pgm = [ 0xE9; 0x02 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1002 inst: SBC #%02X |}]
;;

let%expect_test "testing SBC Binary IMMEDIATE (0xE9) With Borrow" =
  let cycles = 2 in
  let pgm = [ 0xE9; 0x02 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x05; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x00 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x02 y: 0x03 sp: 0xFD sr: nv-bdizC pc: 0x1002 inst: SBC #%02X |}]
;;

let%expect_test "testing SBC Binary IMMEDIATE (0xE9) Underflow" =
  let cycles = 2 in
  let pgm = [ 0xE9; 0x02 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x01; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x02 y: 0x03 sp: 0xFD sr: Nv-bdizc pc: 0x1002 inst: SBC #%02X |}]
;;

let%expect_test "testing SBC Binary IMMEDIATE (0xE9) Overflow" =
  let cycles = 2 in
  let pgm = [ 0xE9; 0x01 ] in
  let computer = init_test_computer pgm in
  let computer =
    { computer with
      cpu = { computer.cpu with a = 0x80; x = 0x02; y = 0x03; sp = 0xFD; sr = 0x01 }
    }
  in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x02 y: 0x03 sp: 0xFD sr: nV-bdizC pc: 0x1002 inst: SBC #%02X |}]
;;
