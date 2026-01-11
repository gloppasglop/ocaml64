(* open Sexplib.Std (* Required for the underlying sexp conversions *) *)
open Base
open Stdio

type bus =
  { address : int
  ; data : int
  }

module CPU_6510 = struct
  type _cpu =
    { (* Pins *)
      rdy : bool (* Ready *)
    ; irq : bool (* IRQ - Inverted *)
    ; nmi : bool (* Non Maskable Interrrupt - Inverted *)
    ; phy1 : bool (* phy1 , IN*)
    ; phy2 : bool (* phy2 , OUT*)
    ; aec : bool (* Address Enable Control *)
    ; rw : bool (* R/W Read/Write *)
    ; reset : bool (* Reset - Inverted *)
    ; address : int (* Pins A0 to A15 *)
    ; data : int (* Pins DB0 to DB7 *)
    ; ioport : int (* Pins P0 to  P7 *)
    ; a : int (* A register *)
    ; x : int (* X register *)
    ; y : int (* Y register *)
    ; sr : int (* Status register *)
    ; pc : int (* Program counter *)
    ; sp : int (* Stack pointer *)
    ; ir : decoded_instruction
    ; cycle : int
      (* Internal instruction cycle count *)
      (* Internal instruction register TODO: Should it be directly teh decoded instruction *)
    }
  [@@deriving sexp]

  and decoded_instruction =
    { inst : _instruction
    ; mode : addressingmode
    ; bytes : int
    ; cycles : int
    }
  [@@deriving sexp]

  and _instruction =
    | ADC
    | AND
    | ASL
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BRK
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | LDA
    | LDX
    | LDY
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROL
    | ROR
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA
    | STX
    | STY
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA
  [@@deriving sexp]

  and addressingmode =
    | ACCUMULATOR
    | ABSOLUTE
    | ABSOLUTEX
    | ABSOLUTEY
    | IMMEDIATE
    | IMPLIED
    | INDIRECT
    | INDEXEDINDIRECT (* X-Indexed, indirect*)
    | INDIRECTINDEXED (* Indirect , Y-Indexed*)
    | RELATIVE
    | ZEROPAGE
    | ZEROPAGEX
    | ZEROPAGEY
  [@@deriving sexp]

  let instruction_to_string = function
    | ADC -> "ADC"
    | AND -> "AND"
    | ASL -> "ASL"
    | BCC -> "BCC"
    | BCS -> "BCS"
    | BEQ -> "BEQ"
    | BIT -> "BIT"
    | BMI -> "BMI"
    | BNE -> "BNE"
    | BPL -> "BPL"
    | BRK -> "BRK"
    | BVC -> "BVC"
    | BVS -> "BVS"
    | CLC -> "CLC"
    | CLD -> "CLD"
    | CLI -> "CLI"
    | CLV -> "CLV"
    | CMP -> "CMP"
    | CPX -> "CPX"
    | CPY -> "CPY"
    | DEC -> "DEC"
    | DEX -> "DEX"
    | DEY -> "DEY"
    | EOR -> "EOR"
    | INC -> "INC"
    | INX -> "INX"
    | INY -> "INY"
    | JMP -> "JMP"
    | JSR -> "JSR"
    | LDA -> "LDA"
    | LDX -> "LDX"
    | LDY -> "LDY"
    | LSR -> "LSR"
    | NOP -> "NOP"
    | ORA -> "ORA"
    | PHA -> "PHA"
    | PHP -> "PHP"
    | PLA -> "PLA"
    | PLP -> "PLP"
    | ROL -> "ROL"
    | ROR -> "ROR"
    | RTI -> "RTI"
    | RTS -> "RTS"
    | SBC -> "SBC"
    | SEC -> "SEC"
    | SED -> "SED"
    | SEI -> "SEI"
    | STA -> "STA"
    | STX -> "STX"
    | STY -> "STY"
    | TAX -> "TAX"
    | TAY -> "TAY"
    | TSX -> "TSX"
    | TXA -> "TXA"
    | TXS -> "TXS"
    | TYA -> "TYA"
  [@@deriving sexp]
  ;;

  let inst_to_string inst mode =
    match mode with
    | ACCUMULATOR -> Printf.sprintf "%s A" (instruction_to_string inst)
    | ABSOLUTE -> Printf.sprintf "%s $%%4X" (instruction_to_string inst)
    | ABSOLUTEX -> Printf.sprintf "%s $%%4X,X" (instruction_to_string inst)
    | ABSOLUTEY -> Printf.sprintf "%s $%%4X,Y" (instruction_to_string inst)
    | IMMEDIATE -> Printf.sprintf "%s #%%2X" (instruction_to_string inst)
    | IMPLIED -> instruction_to_string inst
    | INDIRECT -> Printf.sprintf "%s ($%%4X)" (instruction_to_string inst)
    | INDEXEDINDIRECT -> Printf.sprintf "%s ($%%2X,X)" (instruction_to_string inst)
    | INDIRECTINDEXED -> Printf.sprintf "%s ($%%2X),Y" (instruction_to_string inst)
    | RELATIVE -> Printf.sprintf "%s $%%2X" (instruction_to_string inst)
    | ZEROPAGE -> Printf.sprintf "%s $%%2X" (instruction_to_string inst)
    | ZEROPAGEX -> Printf.sprintf "%s $%%2X,X" (instruction_to_string inst)
    | ZEROPAGEY -> Printf.sprintf "%s $%%2X,Y" (instruction_to_string inst)
  ;;

  let decode inst =
    let mnemonic, mode, bytes, cycles =
      match inst with
      | 0x00 -> BRK, IMPLIED, 1, 7
      | 0xEA -> NOP, IMPLIED, 1, 2
      | 0x2A -> ROL, ACCUMULATOR, 1, 2
      | 0xA9 -> LDA, IMMEDIATE, 2, 2
      | 0xA5 -> LDA, ZEROPAGE, 2, 3
      | 0xB5 -> LDA, ZEROPAGEX, 2, 4
      | 0xAD -> LDA, ABSOLUTE, 3, 4
      | 0xBD -> LDA, ABSOLUTEX, 3, 4
      | 0xB9 -> LDA, ABSOLUTEY, 3, 4
      | 0xA1 -> LDA, INDEXEDINDIRECT, 2, 6
      | 0xB1 -> LDA, INDIRECTINDEXED, 2, 5
      | 0xA2 -> LDX, IMMEDIATE, 2, 2
      | 0xB6 -> LDX, ZEROPAGEY, 2, 4
      | 0x6C -> JMP, INDIRECT, 3, 5
      | 0x90 -> BCC, RELATIVE, 2, 2
      | opcode -> failwith (Printf.sprintf "Opcode %02X Not implemented" opcode)
    in
    { inst = mnemonic; mode; bytes; cycles }
  ;;

  (* Set the Negative and zero flag depending on data value *)
  let set_nz sr data =
    (* Status Register Flags (bit 7 to bit 0) *)
    (* N	Negative *)
    (* V	Overflow *)
    (* -	ignored *)
    (* B	Break *)
    (* D	Decimal (use BCD for arithmetics) *)
    (* I	Interrupt (IRQ disable) *)
    (* Z	Zero *)
    (* C	Carry *)
    let sr = if data = 0 then sr lor 0b0000_0010 else sr land lnot 0b0000_0010 in
    if data land 0b1000_0000 = 0b1000_0000
    then sr lor 0b1000_0000
    else sr land lnot 0b1000_0000
  ;;

  (* TODO : handle R/W and AEC *)

  let tick cpu =
    match cpu.cycle with
    | 1 ->
      let opcode = cpu.data in
      let pc = cpu.pc + 1 in
      { cpu with ir = decode opcode; address = pc; pc; rw = true; cycle = 2 }
    | 2 ->
      (match cpu.ir.mode with
       | IMPLIED -> { cpu with address = cpu.pc; rw = true; cycle = 1 }
       | IMMEDIATE ->
         let data = cpu.data in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | LDA ->
            { cpu with
              a = data
            ; sr = set_nz cpu.sr data
            ; pc
            ; address = pc
            ; rw = true
            ; cycle = 1
            }
          | LDX ->
            { cpu with
              x = data
            ; sr = set_nz cpu.sr data
            ; pc
            ; address = pc
            ; rw = true
            ; cycle = 1
            }
          | _ -> failwith "Not implemented")
       | _ -> failwith "Not implemented")
    | _ -> failwith "Unimplemented"
  ;;
end

open CPU_6510

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

let cpu_to_string cpu =
  printf
    "cycle: %d rw: %b data: %2X address: %04X a: %02X x: %02X y: %02X sr: %02X pc: %02X \
     inst: %s\n"
    cpu.cycle
    cpu.rw
    cpu.data
    cpu.address
    cpu.a
    cpu.x
    cpu.y
    cpu.sr
    cpu.pc
    (inst_to_string cpu.ir.inst cpu.ir.mode)
;;

let () =
  let s = sexp_of__cpu cpu in
  let o = Sexplib.Sexp.to_string_hum s in
  printf "%s\n" o
;;

let mem = Array.create ~len:65536 0xFF
let pgm = [ 0xEA; 0xA9; 0x09; 0xA9; 0x00; 0xA9; 0x89; 0xA2; 0x09; 0xA2; 0x00; 0xA2; 0x89 ]
let load_pgm mem offset pgm = List.iteri pgm ~f:(fun i data -> mem.(offset + i) <- data)
let () = load_pgm mem 0x1000 pgm

(* let () = mem.(0x1000) <- 0xEA *)
(* let () = mem.(0x1001) <- 0xA9 *)
(* let () = mem.(0x1002) <- 0xFF *)
let bus = { address = 0; data = 0 } [@@warning "-32"]

let fetch_decode_execute cpu =
  (* During the first cycle of in struction we need to read from memory we take the value on the bus put it on the data pins of the cpu *)
  if cpu.rw
  then (
    let data = mem.(cpu.address) in
    let bus = { data; address = cpu.address } in
    let cpu = { cpu with data = bus.data; address = bus.address } in
    printf "--- Reading from address 0x%04X : Value 0x%02X\n" bus.address bus.data;
    tick cpu, { data; address = cpu.address })
  else (
    let data = cpu.data in
    let address = cpu.address in
    let bus = { data; address } in
    printf "--- Writing to address 0x%04X : Value 0x%02X\n" bus.address bus.data;
    mem.(bus.address) <- bus.data;
    tick cpu, bus)
;;

let () = cpu_to_string cpu

let rec loop cpu =
  let cpu, _ = fetch_decode_execute cpu in
  cpu_to_string cpu;
  loop cpu
;;

let () = loop cpu
