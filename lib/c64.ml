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
        ; sp = 0x00
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
    {|
    ab: 0x1001 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1001 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1001 inst: NOP
    |}]
;;

let%expect_test "testing LDA IMPLICIT (0xA9) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x01 ] in
  let computer = init_test_computer pgm in
  (* printf "%s" (Cpu.cpu_to_string computer.cpu); *)
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: LDA #%2X |}]
;;

let%expect_test "testing LDA IMPLICIT (0xA9) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x81 ] in
  let computer = init_test_computer pgm in
  (* printf "%s" (Cpu.cpu_to_string computer.cpu); *)
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x81 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: LDA #%2X |}]
;;

let%expect_test "testing LDA IMPLICIT (0xA9) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA9; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with a = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDA #%2X |}]
;;

let%expect_test "testing LDX IMPLICIT (0xA2) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x01 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x01 y: 0x00 sr: 0x00 pc: 0x1002 inst: LDX #%2X |}]
;;

let%expect_test "testing LDX IMPLICIT (0xA2) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x81 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x81 y: 0x00 sr: 0x80 pc: 0x1002 inst: LDX #%2X |}]
;;

let%expect_test "testing LDX IMPLICIT (0xA2) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA2; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with x = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDX #%2X |}]
;;

let%expect_test "testing LDY IMPLICIT (0xA0) non-zero positive" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x01 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x01 sr: 0x00 pc: 0x1002 inst: LDY #%2X |}]
;;

let%expect_test "testing LDY IMPLICIT (0xA0) non-zero negative" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x81 ] in
  let computer = init_test_computer pgm in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x81 sr: 0x80 pc: 0x1002 inst: LDY #%2X |}]
;;

let%expect_test "testing LDY IMPLICIT (0xA0) zero" =
  (* 2 cycles , 1 byte *)
  let cycles = 2 in
  let pgm = [ 0xA0; 0x00 ] in
  let computer = init_test_computer pgm in
  let computer = { computer with cpu = { computer.cpu with y = 0x01; sr = 0x00 } } in
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDY #%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: LDA $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: LDA $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDA $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x01 y: 0x00 sr: 0x00 pc: 0x1002 inst: LDX $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x80 y: 0x00 sr: 0x80 pc: 0x1002 inst: LDX $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDX $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x01 sr: 0x00 pc: 0x1002 inst: LDY $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x80 sr: 0x80 pc: 0x1002 inst: LDY $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: LDY $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: EOR $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: EOR $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: EOR $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: AND $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: AND $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: AND $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: ORA $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x83 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: ORA $%2X |}]
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
  computer.banks.(0x044) <- 0b0000_0000 (* 0x02 *);
  let executions = execute_cycles cycles computer in
  dump_last_execution executions;
  [%expect
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x02 pc: 0x1002 inst: ORA $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x05 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: ADC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x06 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: ADC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0x03 pc: 0x1002 inst: ADC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x80 x: 0x00 y: 0x00 sr: 0xC0 pc: 0x1002 inst: ADC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sr: 0x41 pc: 0x1002 inst: ADC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: ADC $%2X |}]
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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: STA $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: STX $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: STY $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: ASL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x03 pc: 0x1002 inst: ASL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x80 pc: 0x1002 inst: ASL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: LSR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x03 pc: 0x1002 inst: LSR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: LSR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x80 pc: 0x1002 inst: ROR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x03 pc: 0x1002 inst: ROR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x81 pc: 0x1002 inst: ROR $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: ROL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x03 pc: 0x1002 inst: ROL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x80 pc: 0x1002 inst: ROL $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: INC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x80 pc: 0x1002 inst: INC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x02 pc: 0x1002 inst: INC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: DEC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x80 pc: 0x1002 inst: DEC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x00 pc: 0x1002 inst: DEC $%2X

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
    ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x02 y: 0x03 sr: 0x02 pc: 0x1002 inst: DEC $%2X

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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x03 x: 0x00 y: 0x00 sr: 0x01 pc: 0x1002 inst: SBC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sr: 0x01 pc: 0x1002 inst: SBC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: SBC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x7F x: 0x00 y: 0x00 sr: 0x41 pc: 0x1002 inst: SBC $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x42 x: 0x00 y: 0x00 sr: 0x03 pc: 0x1002 inst: CMP $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0xFF x: 0x00 y: 0x00 sr: 0x81 pc: 0x1002 inst: CMP $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x02 x: 0x00 y: 0x00 sr: 0x80 pc: 0x1002 inst: CMP $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x08 x: 0x00 y: 0x00 sr: 0x00 pc: 0x1002 inst: BIT $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x00 x: 0x00 y: 0x00 sr: 0xC2 pc: 0x1002 inst: BIT $%2X |}]
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
    {| ab: 0x1002 db: 0xFF phy2: 0 cycle: 1 rw: true address: 0x1002 data: 0xFF a: 0x01 x: 0x00 y: 0x00 sr: 0xC2 pc: 0x1002 inst: BIT $%2X |}]
;;
