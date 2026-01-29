module M = struct
  type instruction =
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
  [@@warning "-37"]

  type addressingmode =
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

  type decoded_instruction =
    { inst : instruction
    ; mode : addressingmode
    ; bytes : int
    ; cycles : int
    }
  [@@warning "-69"]
  (* [@@deriving sexp] *)

  type cpu =
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
    ; cycle : int (* Internal PC High and PC Low*)
    ; pcl : int
    ; pch : int
      (* Internal instruction cycle count *)
      (* Internal instruction register TODO: Should it be directly teh decoded instruction *)
    }
  [@@warning "-69"]

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
      | 0x05 -> ORA, ZEROPAGE, 2, 3
      | 0x06 -> ASL, ZEROPAGE, 2, 5
      | 0x25 -> AND, ZEROPAGE, 2, 3
      | 0x26 -> ROL, ZEROPAGE, 2, 5
      | 0x46 -> LSR, ZEROPAGE, 2, 5
      | 0x65 -> ADC, ZEROPAGE, 2, 3
      | 0x66 -> ROR, ZEROPAGE, 2, 5
      | 0x84 -> STY, ZEROPAGE, 2, 3
      | 0x85 -> STA, ZEROPAGE, 2, 3
      | 0x86 -> STX, ZEROPAGE, 2, 3
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
      | 0xA6 -> LDX, ZEROPAGE, 2, 3
      | 0xB6 -> LDX, ZEROPAGEY, 2, 4
      | 0xA0 -> LDY, IMMEDIATE, 2, 2
      | 0xA4 -> LDY, ZEROPAGE, 2, 3
      | 0x6C -> JMP, INDIRECT, 3, 5
      | 0x45 -> EOR, ZEROPAGE, 2, 3
      | 0x90 -> BCC, RELATIVE, 2, 2
      | opcode -> failwith (Printf.sprintf "Opcode 0x%02X Not implemented" opcode)
    in
    { inst = mnemonic; mode; bytes; cycles }
  ;;

  let cpu_to_string cpu =
    Printf.sprintf
      "phy2: %d cycle: %d rw: %b address: 0x%04X data: 0x%02X a: 0x%02X x: 0x%02X y: \
       0x%02X sr: 0x%02X pc: 0x%02X inst: %s\n"
      (if cpu.phy2 then 1 else 0)
      cpu.cycle
      cpu.rw
      cpu.address
      cpu.data
      cpu.a
      cpu.x
      cpu.y
      cpu.sr
      cpu.pc
      (inst_to_string cpu.ir.inst cpu.ir.mode)
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

  (* TODO : Decimal mode *)
  (* Overflow is set if: *)
  (* Positive + Positive = Negative *)
  (* or *)
  (* Negative + Negative = Positive *)
  let adc a m sr =
    let res = a + m + (sr land 0x01) in
    (* let carry = res land 0b1_0000_0000 = 0b1_0000_0000 in *)
    let carry = (res land 0b1_0000_0000) lsr 8 in
    let overflow =
      if a land 0b1000_0000 = m land 0b1000_0000
      then not (a land 0b1000_0000 = res land 0b1000_0000)
      else false
    in
    let res = res land 0xFF in
    let sr1 = set_nz sr res in
    (* let sr2 = if carry then sr1 lor 0b0000_0001 else sr1 land 0b1111_1110 in *)
    let sr2 = sr1 land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    let sr3 = if overflow then sr2 lor 0b0100_0000 else sr2 land 0b1011_1111 in
    res land 0xFF, sr3
  ;;

  let inst_asl a sr =
    let res = a lsl 1 in
    let carry = (res land 0b1_0000_0000) lsr 8 in
    let res = res land 0xFF in
    let sr = set_nz sr res in
    let sr = sr land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    res, sr
  ;;

  let inst_rol a sr =
    let current_carry = sr land 0b0000_0001 in
    let res = a lsl 1 in
    let carry = (res land 0b1_0000_0000) lsr 8 in
    let res = res land 0xFF land lnot 0b0000_0001 lor (current_carry land 0b0000_0001) in
    let sr = set_nz sr res in
    let sr = sr land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    res, sr
  ;;

  let inst_lsr a sr =
    let res = a lsr 1 in
    let carry = a land 0b0000_0001 in
    let res = res land 0xFF in
    let sr = set_nz sr res in
    let sr = sr land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    res, sr
  ;;

  let inst_ror a sr =
    let current_carry = (sr land 0b0000_0001) lsl 7 in
    let res = (a lsr 1) land lnot 0b1000_0000 lor (current_carry land 0b1000_0000) in
    let carry = a land 0b0000_0001 in
    let res = res land 0xFF in
    let sr = set_nz sr res in
    let sr = sr land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    res, sr
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
       | ZEROPAGE ->
         let operand = cpu.data in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | STA -> { cpu with address = operand; data = cpu.a; pc; rw = false; cycle = 3 }
          | STX -> { cpu with address = operand; data = cpu.x; pc; rw = false; cycle = 3 }
          | STY -> { cpu with address = operand; data = cpu.y; pc; rw = false; cycle = 3 }
          | LDA | LDX | LDY | AND | ORA | EOR | ADC | ASL | LSR | ROR | ROL ->
            { cpu with address = operand; pc; rw = true; cycle = 3 }
          | _ -> failwith "ZEROPAGE 2 Not implemnetd")
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
          | LDY ->
            { cpu with
              y = data
            ; sr = set_nz cpu.sr data
            ; pc
            ; address = pc
            ; rw = true
            ; cycle = 1
            }
          | _ -> failwith "Not implemented")
       | _ -> failwith "Not implemented")
    | 3 ->
      (match cpu.ir.mode with
       | ZEROPAGE ->
         (match cpu.ir.inst with
          | LDA ->
            let data = cpu.data in
            { cpu with
              a = data
            ; sr = set_nz cpu.sr data
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | LDX ->
            let data = cpu.data in
            { cpu with
              x = data
            ; sr = set_nz cpu.sr data
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | LDY ->
            let data = cpu.data in
            { cpu with
              y = data
            ; sr = set_nz cpu.sr data
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | EOR ->
            let data = cpu.data in
            let a = cpu.a lxor data in
            { cpu with a; sr = set_nz cpu.sr a; address = cpu.pc; rw = true; cycle = 1 }
          | AND ->
            let data = cpu.data in
            let a = cpu.a land data in
            { cpu with a; sr = set_nz cpu.sr a; address = cpu.pc; rw = true; cycle = 1 }
          | ORA ->
            let data = cpu.data in
            let a = cpu.a lor data in
            { cpu with a; sr = set_nz cpu.sr a; address = cpu.pc; rw = true; cycle = 1 }
          | ADC ->
            let data = cpu.data in
            let a, sr = adc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | STA ->
            let data = cpu.a in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STX ->
            let data = cpu.x in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STY ->
            let data = cpu.y in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | ASL | LSR | ROR | ROL -> { cpu with data = cpu.data; rw = false; cycle = 4 }
          | _ -> failwith "Not implemened")
       | _ -> failwith "Addressing Not implemented")
    | 4 ->
      (match cpu.ir.mode with
       | ZEROPAGE ->
         (match cpu.ir.inst with
          | ASL ->
            let data, sr = inst_asl cpu.data cpu.sr in
            { cpu with data; sr; rw = false; cycle = 5 }
          | LSR ->
            let data, sr = inst_lsr cpu.data cpu.sr in
            { cpu with data; sr; rw = false; cycle = 5 }
          | ROR ->
            let data, sr = inst_ror cpu.data cpu.sr in
            { cpu with data; sr; rw = false; cycle = 5 }
          | ROL ->
            let data, sr = inst_rol cpu.data cpu.sr in
            { cpu with data; sr; rw = false; cycle = 5 }
          | _ -> failwith "Inst not implemented")
       | _ -> failwith "Addressing not implemented")
    | 5 ->
      (match cpu.ir.mode with
       | ZEROPAGE ->
         (match cpu.ir.inst with
          | ASL | LSR | ROR | ROL -> { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Inst not implemented")
       | _ -> failwith "Addressing not implemented")
    | 6 -> failwith "Cycle 6 Unimplemented"
    | 7 -> failwith "Cycle 7 Unimplemented"
    | _ -> failwith "Cycle Unimplemented"
  ;;
end
