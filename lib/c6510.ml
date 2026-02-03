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
    | ABSOLUTE -> Printf.sprintf "%s $%%04X" (instruction_to_string inst)
    | ABSOLUTEX -> Printf.sprintf "%s $%%04X,X" (instruction_to_string inst)
    | ABSOLUTEY -> Printf.sprintf "%s $%%04X,Y" (instruction_to_string inst)
    | IMMEDIATE -> Printf.sprintf "%s #%%02X" (instruction_to_string inst)
    | IMPLIED -> instruction_to_string inst
    | INDIRECT -> Printf.sprintf "%s ($%%04X)" (instruction_to_string inst)
    | INDEXEDINDIRECT -> Printf.sprintf "%s ($%%02X,X)" (instruction_to_string inst)
    | INDIRECTINDEXED -> Printf.sprintf "%s ($%%02X),Y" (instruction_to_string inst)
    | RELATIVE -> Printf.sprintf "%s $%%02X" (instruction_to_string inst)
    | ZEROPAGE -> Printf.sprintf "%s $%%02X" (instruction_to_string inst)
    | ZEROPAGEX -> Printf.sprintf "%s $%%02X,X" (instruction_to_string inst)
    | ZEROPAGEY -> Printf.sprintf "%s $%%02X,Y" (instruction_to_string inst)
  ;;

  let decode inst =
    let mnemonic, mode, bytes, cycles =
      match inst with
      | 0x00 -> BRK, IMPLIED, 1, 7
      (* | 0x01 -> BPL, RELATIVE, 0, 0 *)
      (* | 0x02 -> JSR, ABSOLUTE, 0, 0 *)
      (* | 0x03 -> BMI, RELATIVE, 0, 0 *)
      (* | 0x04 -> RTI, IMPLIED, 0, 0 *)
      | 0x05 -> ORA, ZEROPAGE, 2, 3
      | 0x06 -> ASL, ZEROPAGE, 2, 5
      | 0x08 -> PHP, IMPLIED, 1, 3
      | 0x09 -> ORA, IMMEDIATE, 2, 2
      | 0x0A -> ASL, ACCUMULATOR, 1, 2
      | 0x0D -> ORA, ABSOLUTE, 3, 4
      | 0x0E -> ASL, ABSOLUTE, 3, 6
      | 0x18 -> CLC, IMPLIED, 1, 2
      | 0x24 -> BIT, ZEROPAGE, 2, 3
      | 0x25 -> AND, ZEROPAGE, 2, 3
      | 0x26 -> ROL, ZEROPAGE, 2, 5
      | 0x28 -> PLP, IMPLIED, 1, 4
      | 0x29 -> AND, IMMEDIATE, 2, 2
      | 0x2A -> ROL, ACCUMULATOR, 1, 2
      | 0x2C -> BIT, ABSOLUTE, 3, 4
      | 0x2D -> AND, ABSOLUTE, 3, 4
      | 0x2E -> ROL, ABSOLUTE, 3, 6
      | 0x38 -> SEC, IMPLIED, 1, 2
      | 0x45 -> EOR, ZEROPAGE, 2, 3
      | 0x46 -> LSR, ZEROPAGE, 2, 5
      | 0x48 -> PHA, IMPLIED, 1, 3
      | 0x49 -> EOR, IMMEDIATE, 2, 2
      | 0x4A -> LSR, ACCUMULATOR, 1, 2
      | 0x4C -> JMP, ABSOLUTE, 3, 3
      | 0x4D -> EOR, ABSOLUTE, 3, 4
      | 0x4E -> LSR, ABSOLUTE, 3, 6
      | 0x58 -> CLI, IMPLIED, 1, 2
      | 0x65 -> ADC, ZEROPAGE, 2, 3
      | 0x66 -> ROR, ZEROPAGE, 2, 5
      | 0x68 -> PLA, IMPLIED, 1, 4
      | 0x69 -> ADC, IMMEDIATE, 2, 2
      | 0x6A -> ROR, ACCUMULATOR, 1, 2
      | 0x6C -> JMP, INDIRECT, 3, 5
      | 0x6D -> ADC, ABSOLUTE, 3, 4
      | 0x6E -> ROR, ABSOLUTE, 3, 6
      | 0x78 -> SEI, IMPLIED, 1, 2
      | 0x84 -> STY, ZEROPAGE, 2, 3
      | 0x85 -> STA, ZEROPAGE, 2, 3
      | 0x86 -> STX, ZEROPAGE, 2, 3
      | 0x88 -> DEY, IMPLIED, 1, 2
      | 0x8A -> TXA, IMPLIED, 1, 2
      | 0x8C -> STY, ABSOLUTE, 3, 4
      | 0x8D -> STA, ABSOLUTE, 3, 4
      | 0x8E -> STX, ABSOLUTE, 3, 4
      | 0x90 -> BCC, RELATIVE, 2, 2
      | 0x98 -> TYA, IMPLIED, 1, 2
      | 0x9A -> TXS, IMPLIED, 1, 2
      | 0xA0 -> LDY, IMMEDIATE, 2, 2
      | 0xA1 -> LDA, INDEXEDINDIRECT, 2, 6
      | 0xA2 -> LDX, IMMEDIATE, 2, 2
      | 0xA4 -> LDY, ZEROPAGE, 2, 3
      | 0xA5 -> LDA, ZEROPAGE, 2, 3
      | 0xA6 -> LDX, ZEROPAGE, 2, 3
      | 0xA8 -> TAY, IMPLIED, 1, 2
      | 0xA9 -> LDA, IMMEDIATE, 2, 2
      | 0xAA -> TAX, IMPLIED, 1, 2
      | 0xAD -> LDA, ABSOLUTE, 3, 4
      | 0xAE -> LDX, ABSOLUTE, 3, 4
      | 0xAC -> LDY, ABSOLUTE, 3, 4
      | 0xB1 -> LDA, INDIRECTINDEXED, 2, 5
      | 0xB5 -> LDA, ZEROPAGEX, 2, 4
      | 0xB6 -> LDX, ZEROPAGEY, 2, 4
      | 0xB8 -> CLV, IMPLIED, 1, 2
      | 0xB9 -> LDA, ABSOLUTEY, 3, 4
      | 0xBA -> TSX, IMPLIED, 1, 2
      | 0xBD -> LDA, ABSOLUTEX, 3, 4
      | 0xC0 -> CPY, IMMEDIATE, 2, 2
      | 0xC4 -> CPY, ZEROPAGE, 2, 3
      | 0xC5 -> CMP, ZEROPAGE, 2, 3
      | 0xC6 -> DEC, ZEROPAGE, 2, 5
      | 0xC8 -> INY, IMPLIED, 1, 2
      | 0xC9 -> CMP, IMMEDIATE, 2, 2
      | 0xCA -> DEX, IMPLIED, 1, 2
      | 0xCC -> CPY, ABSOLUTE, 3, 4
      | 0xCD -> CMP, ABSOLUTE, 3, 4
      | 0xCE -> DEC, ABSOLUTE, 3, 6
      | 0xD8 -> CLD, IMPLIED, 1, 2
      | 0xE0 -> CPX, IMMEDIATE, 2, 2
      | 0xE4 -> CPX, ZEROPAGE, 2, 3
      | 0xE5 -> SBC, ZEROPAGE, 2, 3
      | 0xE6 -> INC, ZEROPAGE, 2, 5
      | 0xE8 -> INX, IMPLIED, 1, 2
      | 0xEA -> NOP, IMPLIED, 1, 2
      | 0xEC -> CPX, ABSOLUTE, 3, 4
      | 0xEE -> INC, ABSOLUTE, 3, 6
      | 0xF8 -> SED, IMPLIED, 1, 2
      | opcode -> failwith (Printf.sprintf "Opcode 0x%02X Not implemented" opcode)
    in
    { inst = mnemonic; mode; bytes; cycles }
  ;;

  let sr_to_string sr =
    (*nv‑BdIZc*)
    let n = if sr land 0b1000_0000 = 0b1000_0000 then "N" else "n" in
    let v = if sr land 0b0100_0000 = 0b0100_0000 then "V" else "v" in
    let b = if sr land 0b0001_0000 = 0b0001_0000 then "B" else "b" in
    let d = if sr land 0b0000_1000 = 0b0000_1000 then "D" else "d" in
    let i = if sr land 0b0000_0100 = 0b0000_0100 then "I" else "i" in
    let z = if sr land 0b0000_0010 = 0b0000_0010 then "Z" else "z" in
    let c = if sr land 0b0000_0001 = 0b0000_0001 then "C" else "c" in
    String.concat "" [ n; v; "-"; b; d; i; z; c ]
  ;;

  let cpu_to_string cpu =
    Printf.sprintf
      "phy2: %d cycle: %d rw: %b address: 0x%04X data: 0x%02X a: 0x%02X x: 0x%02X y: \
       0x%02X sp: 0x%02X sr: %8s pc: 0x%02X inst: %s\n"
      (if cpu.phy2 then 1 else 0)
      cpu.cycle
      cpu.rw
      cpu.address
      cpu.data
      cpu.a
      cpu.x
      cpu.y
      cpu.sp
      (sr_to_string cpu.sr)
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
  let inst_adc a m sr =
    let res = a + m + (sr land 0x01) in
    let carry = (res land 0b1_0000_0000) lsr 8 in
    let res = res land 0xFF in
    let overflow = (a lxor res land (m lxor res) land 0x80) lsr 1 in
    let res = res land 0xFF in
    let sr1 = set_nz sr res in
    let sr2 = sr1 land lnot 0b0000_0001 lor (carry land 0b0000_0001) in
    let sr3 = sr2 land lnot 0b0100_0000 lor (overflow land 0b0100_0000) in
    res land 0xFF, sr3
  ;;

  let inst_sbc a m sr =
    let m = (0xFF - m) land 0xFF in
    inst_adc a m sr
  ;;

  let inst_bit a m sr =
    let res = a land m in
    let sr = if res = 0 then sr lor 0b0000_0010 else sr land lnot 0b0000_0010 in
    let sr = sr land lnot 0b1100_0000 lor (m land 0b1100_0000) in
    a, sr
  ;;

  (* let inst_cmp a m _ = *)
  (* let res = (a - m) land 0xFF in *)
  (* let carry = if a >= m then 0b0000_0001 else 0b0000_0000 in *)
  (* res, set_nz carry res *)
  (* ;; *)

  let inst_cmp a m sr =
    let sr' = sr land lnot 0b0000_0001 lor 0b0000_0001 in
    inst_sbc a m sr'
  ;;

  let inst_inc m sr =
    let res = (m + 1) land 0xFF in
    let sr = set_nz sr res in
    res, sr
  ;;

  let inst_dec m sr =
    let res = (m - 1) land 0xFF in
    let sr = set_nz sr res in
    res, sr
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
       | IMPLIED ->
         (match cpu.ir.inst with
          | NOP -> { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | CLC ->
            { cpu with
              sr = cpu.sr land lnot 0b0000_0001
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | SEC ->
            { cpu with
              sr = cpu.sr lor 0b0000_0001
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | CLD ->
            { cpu with
              sr = cpu.sr land lnot 0b0000_1000
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | SED ->
            { cpu with
              sr = cpu.sr lor 0b0000_1000
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | CLI ->
            { cpu with
              sr = cpu.sr land lnot 0b0000_0100
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | SEI ->
            { cpu with
              sr = cpu.sr lor 0b0000_0100
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | CLV ->
            { cpu with
              sr = cpu.sr land lnot 0b0100_0000
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | INX ->
            let x, sr = inst_inc cpu.x cpu.sr in
            { cpu with x; sr; address = cpu.pc; rw = true; cycle = 1 }
          | INY ->
            let y, sr = inst_inc cpu.y cpu.sr in
            { cpu with y; sr; address = cpu.pc; rw = true; cycle = 1 }
          | DEX ->
            let x, sr = inst_dec cpu.x cpu.sr in
            { cpu with x; sr; address = cpu.pc; rw = true; cycle = 1 }
          | DEY ->
            let y, sr = inst_dec cpu.y cpu.sr in
            { cpu with y; sr; address = cpu.pc; rw = true; cycle = 1 }
          | TAX ->
            { cpu with
              x = cpu.a
            ; sr = set_nz cpu.sr cpu.a
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | TXA ->
            { cpu with
              a = cpu.x
            ; sr = set_nz cpu.sr cpu.x
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | TAY ->
            { cpu with
              y = cpu.a
            ; sr = set_nz cpu.sr cpu.a
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | TYA ->
            { cpu with
              a = cpu.y
            ; sr = set_nz cpu.sr cpu.y
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | TXS ->
            { cpu with
              sp = cpu.x
            ; sr = set_nz cpu.sr cpu.x
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | TSX ->
            { cpu with
              x = cpu.sp
            ; sr = set_nz cpu.sr cpu.sp
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | PHA ->
            let sp = (cpu.sp - 1) land 0xFF in
            { cpu with address = 0x100 + cpu.sp; data = cpu.a; sp; rw = false; cycle = 3 }
          | PLA ->
            let sp = (cpu.sp + 1) land 0xFF in
            { cpu with address = 0x100 + cpu.sp; sp; rw = true; cycle = 3 }
          | PLP ->
            let sp = (cpu.sp + 1) land 0xFF in
            { cpu with address = 0x100 + cpu.sp; sp; rw = true; cycle = 3 }
          | PHP ->
            let sp = (cpu.sp - 1) land 0xFF in
            { cpu with
              address = 0x100 + cpu.sp
            ; data = cpu.sr
            ; sp
            ; rw = false
            ; cycle = 3
            }
          | _ -> failwith "Not Implemented")
       | ACCUMULATOR ->
         let a, sr =
           match cpu.ir.inst with
           | ASL -> inst_asl cpu.a cpu.sr
           | LSR -> inst_lsr cpu.a cpu.sr
           | ROR -> inst_ror cpu.a cpu.sr
           | ROL -> inst_rol cpu.a cpu.sr
           | _ -> failwith "Invalid Accumulator opcode"
         in
         { cpu with a; sr; rw = true; cycle = 1 }
       | ZEROPAGE ->
         let operand = cpu.data in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | STA -> { cpu with address = operand; data = cpu.a; pc; rw = false; cycle = 3 }
          | STX -> { cpu with address = operand; data = cpu.x; pc; rw = false; cycle = 3 }
          | STY -> { cpu with address = operand; data = cpu.y; pc; rw = false; cycle = 3 }
          | LDA
          | LDX
          | LDY
          | AND
          | ORA
          | EOR
          | ADC
          | SBC
          | ASL
          | LSR
          | ROR
          | INC
          | ROL
          | DEC
          | CMP
          | CPX
          | CPY
          | BIT -> { cpu with address = operand; pc; rw = true; cycle = 3 }
          | _ -> failwith "ZEROPAGE Cycle 2 Not implemnetd")
       | ZEROPAGEX ->
         let operand = cpu.data in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | STA
          | STY
          | LDA
          | LDY
          | AND
          | ORA
          | EOR
          | ADC
          | SBC
          | ASL
          | LSR
          | ROR
          | INC
          | ROL
          | DEC
          | CMP
          | CPX
          | CPY
          | BIT -> { cpu with address = operand; pc; rw = true; cycle = 3 }
          | _ -> failwith "ZEROPAGEX Cycle 2 Not implemnetd")
       | ABSOLUTE ->
         let pcl = cpu.data in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | STA
          | STX
          | STY
          | LDA
          | LDX
          | LDY
          | AND
          | ORA
          | EOR
          | ADC
          | SBC
          | ASL
          | LSR
          | ROR
          | INC
          | ROL
          | DEC
          | CMP
          | CPX
          | CPY
          | JMP
          | BIT -> { cpu with address = pc; pcl; pc; rw = true; cycle = 3 }
          | _ -> failwith "ABSOLUTE Cycle 2 Not implemnetd")
       | IMMEDIATE ->
         let data = cpu.data in
         let pc = cpu.pc + 1 in
         let a, x, y, sr =
           match cpu.ir.inst with
           | LDA -> data, cpu.x, cpu.y, set_nz cpu.sr data
           | ADC ->
             let a, sr = inst_adc cpu.a data cpu.sr in
             a, cpu.x, cpu.y, sr
           | CMP ->
             let _, sr = inst_cmp cpu.a data cpu.sr in
             cpu.a, cpu.x, cpu.y, sr
           | CPX ->
             let _, sr = inst_cmp cpu.x data cpu.sr in
             cpu.a, cpu.x, cpu.y, sr
           | CPY ->
             let _, sr = inst_cmp cpu.y data cpu.sr in
             cpu.a, cpu.x, cpu.y, sr
           | AND ->
             let a = cpu.a land data in
             a, cpu.x, cpu.y, set_nz cpu.sr a
           | EOR ->
             let a = cpu.a lxor data in
             a, cpu.x, cpu.y, set_nz cpu.sr a
           | ORA ->
             let a = cpu.a lor data in
             a, cpu.x, cpu.y, set_nz cpu.sr a
           | LDX -> cpu.a, data, cpu.y, set_nz cpu.sr data
           | LDY -> cpu.a, cpu.x, data, set_nz cpu.sr data
           | _ -> failwith "Not implemented"
         in
         { cpu with a; x; y; sr; rw = true; address = pc; pc; cycle = 1 }
       | _ -> failwith "Not implemented")
    | 3 ->
      (match cpu.ir.mode with
       | IMPLIED ->
         (match cpu.ir.inst with
          | PLA | PLP -> { cpu with rw = true; cycle = 4 }
          | PHA | PHP -> { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Implied cycle 3 not implemented")
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
            let a, sr = inst_adc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | SBC ->
            let data = cpu.data in
            let a, sr = inst_sbc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | CMP ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPX ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.x data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPY ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.y data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | BIT ->
            let data = cpu.data in
            let _, sr = inst_bit cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | STA ->
            let data = cpu.a in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STX ->
            let data = cpu.x in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STY ->
            let data = cpu.y in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with data = cpu.data; rw = false; cycle = 4 }
          | _ -> failwith "Not implemened")
       | ZEROPAGEX ->
         let operand = cpu.data in
         let pc = cpu.pc in
         let address = (operand + cpu.x) land 0xFF in
         (match cpu.ir.inst with
          | STA -> { cpu with address; data = cpu.a; pc; rw = false; cycle = 4 }
          | STY -> { cpu with address; data = cpu.y; pc; rw = false; cycle = 4 }
          | LDA
          | LDY
          | AND
          | ORA
          | EOR
          | ADC
          | SBC
          | ASL
          | LSR
          | ROR
          | INC
          | ROL
          | DEC
          | CMP
          | CPX
          | CPY
          | BIT -> { cpu with address; pc; rw = true; cycle = 4 }
          | _ -> failwith "ZEROPAGEX Cycle 3 Not implemnetd")
       | ABSOLUTE ->
         let pch = cpu.data in
         let address = (pch lsl 8) lor cpu.pcl in
         let pc = cpu.pc + 1 in
         (match cpu.ir.inst with
          | STA -> { cpu with address; data = cpu.a; pc; rw = false; cycle = 4 }
          | STX -> { cpu with address; data = cpu.x; pc; rw = false; cycle = 4 }
          | STY -> { cpu with address; data = cpu.y; pc; rw = false; cycle = 4 }
          | JMP -> { cpu with address; pc = address; rw = true; cycle = 1 }
          | LDA
          | LDX
          | LDY
          | AND
          | ORA
          | EOR
          | ADC
          | SBC
          | ASL
          | LSR
          | ROR
          | INC
          | ROL
          | DEC
          | CMP
          | CPX
          | CPY
          | BIT -> { cpu with address; pc; rw = true; cycle = 4 }
          | _ -> failwith "ABSOLUTE Cycle 3 Not implemnetd")
       | _ -> failwith "Addressing Not implemented")
    | 4 ->
      (match cpu.ir.mode with
       | IMPLIED ->
         (match cpu.ir.inst with
          | PLA ->
            { cpu with
              a = cpu.data
            ; sr = set_nz cpu.sr cpu.data
            ; address = cpu.pc
            ; rw = true
            ; cycle = 1
            }
          | PLP -> { cpu with sr = cpu.data; address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Inst not implemented")
       | ZEROPAGE ->
         let data, sr =
           match cpu.ir.inst with
           | ASL -> inst_asl cpu.data cpu.sr
           | LSR -> inst_lsr cpu.data cpu.sr
           | ROR -> inst_ror cpu.data cpu.sr
           | ROL -> inst_rol cpu.data cpu.sr
           | INC -> inst_inc cpu.data cpu.sr
           | DEC -> inst_dec cpu.data cpu.sr
           | _ -> failwith "Inst not implemented"
         in
         { cpu with data; sr; rw = false; cycle = 5 }
       | ZEROPAGEX ->
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
            let a, sr = inst_adc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | SBC ->
            let data = cpu.data in
            let a, sr = inst_sbc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | CMP ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPX ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.x data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPY ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.y data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | BIT ->
            let data = cpu.data in
            let _, sr = inst_bit cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | STA ->
            let data = cpu.a in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STX ->
            let data = cpu.x in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STY ->
            let data = cpu.y in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with data = cpu.data; rw = false; cycle = 4 }
          | _ -> failwith "Not implemened")
       | ABSOLUTE ->
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
            let a, sr = inst_adc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | SBC ->
            let data = cpu.data in
            let a, sr = inst_sbc cpu.a data cpu.sr in
            { cpu with a; sr; address = cpu.pc; rw = true; cycle = 1 }
          | CMP ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPX ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.x data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | CPY ->
            let data = cpu.data in
            let _, sr = inst_cmp cpu.y data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | BIT ->
            let data = cpu.data in
            let _, sr = inst_bit cpu.a data cpu.sr in
            { cpu with sr; address = cpu.pc; rw = true; cycle = 1 }
          | STA ->
            let data = cpu.a in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STX ->
            let data = cpu.x in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | STY ->
            let data = cpu.y in
            { cpu with data; address = cpu.pc; rw = true; cycle = 1 }
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with data = cpu.data; rw = false; cycle = 5 }
          | _ -> failwith "Not implemened")
       | _ -> failwith "Addressing not implemented")
    | 5 ->
      (match cpu.ir.mode with
       | ZEROPAGE ->
         (match cpu.ir.inst with
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Inst not implemented")
       | ZEROPAGEX ->
         let data, sr =
           match cpu.ir.inst with
           | ASL -> inst_asl cpu.data cpu.sr
           | LSR -> inst_lsr cpu.data cpu.sr
           | ROR -> inst_ror cpu.data cpu.sr
           | ROL -> inst_rol cpu.data cpu.sr
           | INC -> inst_inc cpu.data cpu.sr
           | DEC -> inst_dec cpu.data cpu.sr
           | _ -> failwith "Inst not implemented"
         in
         { cpu with data; sr; rw = false; cycle = 6 }
       | ABSOLUTE ->
         let data, sr =
           match cpu.ir.inst with
           | ASL -> inst_asl cpu.data cpu.sr
           | LSR -> inst_lsr cpu.data cpu.sr
           | ROR -> inst_ror cpu.data cpu.sr
           | ROL -> inst_rol cpu.data cpu.sr
           | INC -> inst_inc cpu.data cpu.sr
           | DEC -> inst_dec cpu.data cpu.sr
           | _ -> failwith "Inst not implemented"
         in
         { cpu with data; sr; rw = false; cycle = 6 }
       | _ -> failwith "Addressing not implemented")
    | 6 ->
      (match cpu.ir.mode with
       | ZEROPAGEX ->
         (match cpu.ir.inst with
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Inst not implemented")
       | ABSOLUTE ->
         (match cpu.ir.inst with
          | ASL | LSR | ROR | ROL | INC | DEC ->
            { cpu with address = cpu.pc; rw = true; cycle = 1 }
          | _ -> failwith "Inst not implemented")
       | _ -> failwith "Addressing not implemented")
    | 7 -> failwith "Cycle 7 Unimplemented"
    | _ -> failwith "Cycle Unimplemented"
  ;;
end
