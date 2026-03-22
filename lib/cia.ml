module Cia = struct
  (*
REG
0   PRA
1   PRB
2   DDRA
3   DDRB
4   TA LO
5   TA HI
6   TB LO
7   TB HI
8   TOD 10TH
9   TOD SEC
A   TOD MIN
B   TOD HR
C   SDR
D   ICR
E   CRA
F   CRB
*)

  type chip =
    { (* REGISTERS *)
      pra : int
    ; prb : int
    ; ddra : int
    ; ddrb : int
    ; talo : int
    ; tahi : int
    ; tblo : int
    ; tbhi : int
    ; tod10th : int
    ; todsec : int
    ; todmin : int
    ; todhr : int
    ; sdr : int
    ; icr : int
    ; cra : int
    ; crb : int (* LATCHES *)
    ; tallo : int
    ; talhi : int
    ; tbllo : int
    ; tblhi : int (* PINS *)
    ; phy2 : bool (* phy2 , OUT*)
    ; csb : bool (*/CS Chip Select *)
    ; rw : bool (* R/W Read/Write *)
    ; resetb : bool (* /RES - Inverted *)
    ; rs : int (* Pins rs0 to rs3 - Register Select *)
    ; data : int (* Pins DB0 to DB7 *)
    ; irqb : bool (* /IRQ *)
    ; pa : int (* PA0-PA7 *)
    ; pb : int (* PB0-PB7 *)
    ; pcb : bool (* /PC *)
    ; flagb : bool (* /FLAG *)
    ; sp : bool (* Serial port buffer *)
    ; cnt : bool (* Stack pointer *)
    }

  let create () =
    { pra = 0
    ; prb = 0
    ; ddra = 0
    ; ddrb = 0
    ; talo = 0
    ; tahi = 0
    ; tblo = 0
    ; tbhi = 0
    ; tod10th = 0
    ; todsec = 0
    ; todmin = 0
    ; todhr = 0
    ; sdr = 0
    ; icr = 0
    ; cra = 0
    ; crb = 0 (* LATCHES *)
    ; tallo = 0
    ; talhi = 0
    ; tbllo = 0
    ; tblhi = 0 (* PINS *)
    ; phy2 = false (* phy2 , OUT*)
    ; csb = true (*/CS Chip Select *)
    ; rw = true (* R/W Read/Write *)
    ; resetb = false (* /RES - Inverted *)
    ; rs = 0 (* Pins rs0 to rs3 - Register Select *)
    ; data = 0
    ; irqb = true (* /IRQ *)
    ; pa = 0 (* PA0-PA7 *)
    ; pb = 0 (* PB0-PB7 *)
    ; pcb = true (* /PC *)
    ; flagb = true (* /FLAG *)
    ; sp = false (* Seria Port Buffer *)
    ; cnt = false
    }
  ;;

  let tick chip =
    let read chip =
      match chip.rs with
      | 0 -> { chip with data = chip.pa; pcb = false }
      | 1 ->
        { chip with data = chip.pb; pcb = true }
        (* TODO: Handle timer than can influence PB7 and PB6 *)
      | 2 -> { chip with data = chip.ddra; pcb = false }
      | 3 -> { chip with data = chip.ddrb; pcb = false }
      | 4 -> { chip with data = chip.talo; pcb = false }
      | 5 -> { chip with data = chip.tahi; pcb = false }
      | 6 -> { chip with data = chip.tblo; pcb = false }
      | 7 -> { chip with data = chip.tbhi; pcb = false }
      | 8 ->
        { chip with data = chip.tod10th; pcb = false }
        (* TODO Read on tod10th should release the latch on TOD *)
      | 9 ->
        { chip with data = chip.todsec; pcb = false }
        (* TODO Should probably use the latchd value *)
      | 10 ->
        { chip with data = chip.todmin; pcb = false }
        (* TODO Should probably use the latchd value *)
      | 11 ->
        { chip with data = chip.todhr; pcb = false }
        (* TODO Read on todhr should latch todh *)
      | 12 -> { chip with data = chip.sdr; pcb = false }
      | 13 ->
        { chip with data = chip.icr land 0b1001_1111; icr = 0; irqb = true; pcb = false }
      | 14 -> { chip with data = chip.cra; pcb = false }
      | 15 -> { chip with data = chip.crb; pcb = false }
      | _ -> failwith "TODO"
    in
    let write chip =
      (* TODO : *)
      match chip.rs with
      | 0 -> { chip with pa = chip.data; pcb = false }
      | 1 ->
        { chip with pb = chip.data; pcb = false }
        (* TODO: Handle timer than can influence PB7 and PB6 *)
      | 2 -> { chip with ddra = chip.data; pcb = false }
      | 3 -> { chip with ddrb = chip.data; pcb = false }
      | 4 -> { chip with talo = chip.data; tallo = chip.data; pcb = false }
      | 5 -> { chip with tahi = chip.data; talhi = chip.data; pcb = false }
      | 6 -> { chip with tblo = chip.data; tbllo = chip.data; pcb = false }
      | 7 -> { chip with tbhi = chip.data; tblhi = chip.data; pcb = false }
      | 8 ->
        { chip with tod10th = chip.data; pcb = false }
        (* TODO More complex than that see datasheet *)
      | 9 ->
        { chip with todsec = chip.data; pcb = false }
        (* TODO More complex than that see datasheet *)
      | 10 ->
        { chip with todmin = chip.data; pcb = false }
        (* TODO More complex than that see datasheet *)
      | 11 ->
        { chip with todhr = chip.data; pcb = false }
        (* TODO More complex than that see datasheet *)
      | 12 ->
        { chip with sdr = chip.data; pcb = false }
        (* TODO More complex than that see datasheet *)
      | 13 ->
        if chip.icr land 0b1000_0000 = 0b1000_0000
        then { chip with icr = chip.data lor chip.icr; irqb = true; pcb = false }
        else { chip with icr = chip.icr land (lnot chip.data land 0xFF); pcb = false }
      (* TODO: Handle forceload read/write *)
      | 14 ->
        (*
          If we start the timer then we should set the toggle/pulse
          high
          We check if start bit is set. If it was already set then we jist
          load data into cra else we set the toggle bit high in the cra
        *)
        let cra =
          if chip.data land 0b0000_0001 = 1
          then
            if chip.cra land 0b0000_0001 = 0 then chip.data lor 0b0000_0100 else chip.data
          else chip.data
        in
        { chip with cra; pcb = false }
      | 15 ->
        let crb =
          if chip.data land 0b0000_0001 = 1
          then
            if chip.crb land 0b0000_0001 = 0 then chip.data lor 0b0000_0100 else chip.data
          else chip.data
        in
        { chip with crb; pcb = false }
      | _ -> failwith "TODO"
    in
    let handle_timer_a chip =
      (* TIMER A *)
      let cra = chip.cra in
      (* Check if force load and timer started *)
      let chip =
        match cra land 0b0001_0000 with
        | 0 ->
          (* This is Not a force load *)
          (* Check Timer IN Mode *)
          let ta =
            match cra land 0b0010_0001 with
            | 0b0000_0001 ->
              (* Count phy2 pulses *)
              if chip.phy2
              then (chip.talo land (chip.tahi lsl 8)) - 1
              else chip.talo land (chip.tahi lsl 8)
            | 0b0010_0001 ->
              (* Count CNT *)
              if chip.cnt
              then (chip.talo land (chip.tahi lsl 8)) - 1
              else chip.talo land (chip.tahi lsl 8)
            | _ ->
              (* Timer is stopped *)
              chip.talo land (chip.tahi lsl 8)
          in
          (* Check if we underflow *)
          if ta < 0
          then (
            (* Underflow *)
            (* PortB output *)
            let pb =
              match cra land 0b0000_0010 with
              | 0 ->
                (* PBON not set *)
                chip.pb
              | _ ->
                (* PBON is set *)
                (match cra land 0b0000_0100 with
                 | 0 ->
                   (* Pulse mode *)
                   chip.pb lor 0b0100_0000
                   (* TODO: this should be the case for one cycle only *)
                 | _ ->
                   (* Toggle mode *)
                   chip.pb lxor 0b0100_0000)
            in
            (* Check Continuous mode *)
            match cra land 0b000_1000 with
            | 0 ->
              (* This is continuous mode *)
              (* reload timerA with latched values *)

              (* set interrupt bit of icr regardless of the mask *)
              (* set irqb to low only if timerA interrupt is not masked *)
              (* set icr bit 8 to high only if timerA interrupt is not masked *)
              let irqb, icr =
                if chip.icr land 0b0000_0001 = 0b0000_0001
                then false, chip.icr lor 0b1000_0001
                else chip.irqb, chip.icr lor 0b0000_0001
              in
              { chip with talo = chip.tallo; tahi = chip.talhi; irqb; pb; icr }, true
            | _ ->
              (* This is on-shot mode. So we stop timer *)
              ( { chip with
                  talo = chip.tallo
                ; tahi = chip.talhi
                ; irqb = true
                ; cra = chip.cra land 0b1111_1110 (* stop timer *)
                ; pb
                }
              , true ))
          else
            (* No underflow, just store decreased timers *)
            { chip with talo = ta land 0xFF; tahi = (ta lsr 8) land 0xFF }, false
        | _ ->
          (* This is a force load *)
          ( { chip with
              talo = chip.tallo
            ; tahi = chip.talhi
            ; cra = chip.cra land 0b1111_1110
            }
          , false )
      in
      chip
    in
    let handle_timer_b (chip, underflow) =
      (* TIMER B *)
      let crb = chip.crb in
      (* Check if force load and timer started *)
      let chip =
        match crb land 0b0001_0000 with
        | 0 ->
          (* This is Not a force load *)
          (* Check Timer IN Mode *)
          let tb =
            match crb land 0b0110_0001 with
            | 0b0000_0001 ->
              (* Count phy2 pulses *)
              if chip.phy2
              then (chip.tblo land (chip.tbhi lsl 8)) - 1
              else chip.tblo land (chip.tbhi lsl 8)
            | 0b0010_0001 ->
              (* Count CNT *)
              if chip.cnt
              then (chip.tblo land (chip.tbhi lsl 8)) - 1
              else chip.tblo land (chip.tbhi lsl 8)
            | 0b0100_0001 ->
              (* Count TA underflow *)
              if underflow
              then (chip.tblo land (chip.tbhi lsl 8)) - 1
              else chip.tblo land (chip.tbhi lsl 8)
            | 0b0110_0001 ->
              (* Count TA underflow while CNT high*)
              if underflow && chip.cnt
              then (chip.tblo land (chip.tbhi lsl 8)) - 1
              else chip.tblo land (chip.tbhi lsl 8)
            | _ ->
              (* Timer is stopped *)
              chip.tblo land (chip.tbhi lsl 8)
          in
          (* Check if we underflow *)
          if tb < 0
          then (
            (* Underflow *)
            (* PortB output *)
            let pb =
              match crb land 0b0000_0010 with
              | 0 ->
                (* PBON not set *)
                chip.pb
              | _ ->
                (* PBON is set *)
                (match crb land 0b0000_0100 with
                 | 0 ->
                   (* Pulse mode *)
                   chip.pb lor 0b1000_0000
                   (* TODO: this should be the case for one cycle only *)
                 | _ ->
                   (* Toggle mode *)
                   chip.pb lxor 0b1000_0000)
            in
            (* set interrupt bit of icr regardless of the mask *)
            (* set irqb to low only if timerA interrupt is not masked *)
            (* set icr bit 8 to high only if timerA interrupt is not masked *)
            let irqb, icr =
              if chip.icr land 0b0000_0010 = 0b0000_0010
              then false, chip.icr lor 0b1000_0010
              else chip.irqb, chip.icr lor 0b0000_0010
            in
            (* Check Continuous mode *)
            match crb land 0b000_1000 with
            | 0 ->
              (* This is continuous mode *)
              (* reload timerA with latched values *)
              { chip with tblo = chip.tbllo; tbhi = chip.tblhi; irqb; pb; icr }
            | _ ->
              (* This is on-shot mode. So we stop timer *)
              { chip with
                tblo = chip.tbllo
              ; tbhi = chip.tblhi
              ; irqb
              ; crb = chip.crb land 0b1111_1110 (* stop timer *)
              ; pb
              ; icr
              })
          else
            (* No underflow, just store decreased timers *)
            { chip with tblo = tb land 0xFF; tbhi = (tb lsr 8) land 0xFF }
        | _ ->
          (* This is a force load *)
          { chip with
            tblo = chip.tbllo
          ; tbhi = chip.tblhi
          ; crb = chip.crb land 0b1111_1110
          }
      in
      chip
    in
    (* Handle read/write *)
    let chip' =
      match chip.csb with
      | true -> chip |> handle_timer_a |> handle_timer_b
      | false ->
        (match chip.phy2 with
         | true ->
           (match chip.rw with
            | true -> chip |> handle_timer_a |> handle_timer_b |> read
            | false -> chip |> write |> handle_timer_a |> handle_timer_b)
         | false -> failwith "Not implemented")
    in
    (* Handle TOD *)
    (* Handle RESET *)
    (* Handle Shift Register *)
    (*Handle /FLAG*)
    chip'
  ;;
end
