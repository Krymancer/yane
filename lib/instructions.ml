(*
 * Instructions.ml - 6502 Instruction Set
 * 
 * This module implements the 6502 instruction set.
 * The 6502 has 56 legal instructions with various addressing modes.
 *)

(* Addressing modes *)
type addressing_mode =
  | Implicit       (* No operand *)
  | Accumulator    (* Operate on accumulator *)
  | Immediate      (* #$nn *)
  | ZeroPage       (* $nn *)
  | ZeroPageX      (* $nn,X *)
  | ZeroPageY      (* $nn,Y *)
  | Relative       (* Branch instructions *)
  | Absolute       (* $nnnn *)
  | AbsoluteX      (* $nnnn,X *)
  | AbsoluteY      (* $nnnn,Y *)
  | Indirect       (* ($nnnn) - JMP only *)
  | IndexedIndirect (* ($nn,X) *)
  | IndirectIndexed (* ($nn),Y *)

(* Instruction type *)
type instruction = {
  opcode : int;
  name : string;
  mode : addressing_mode;
  cycles : int;
  execute : Emulator.emulator -> unit;
}

(* Get the operand address based on addressing mode *)
let get_operand_address emu mode =
  match mode with
  | Immediate -> 
      let addr = emu.Emulator.cpu.pc in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      addr
  | ZeroPage ->
      let addr = Emulator.read_byte emu emu.Emulator.cpu.pc in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      addr
  | ZeroPageX ->
      let addr = (Emulator.read_byte emu emu.Emulator.cpu.pc + emu.Emulator.cpu.x) land 0xFF in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      addr
  | ZeroPageY ->
      let addr = (Emulator.read_byte emu emu.Emulator.cpu.pc + emu.Emulator.cpu.y) land 0xFF in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      addr
  | Absolute ->
      let addr = Emulator.read_word emu emu.Emulator.cpu.pc in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 2;
      addr
  | AbsoluteX ->
      let addr = (Emulator.read_word emu emu.Emulator.cpu.pc + emu.Emulator.cpu.x) land 0xFFFF in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 2;
      addr
  | AbsoluteY ->
      let addr = (Emulator.read_word emu emu.Emulator.cpu.pc + emu.Emulator.cpu.y) land 0xFFFF in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 2;
      addr
  | IndexedIndirect ->
      let zero_page_addr = (Emulator.read_byte emu emu.Emulator.cpu.pc + emu.Emulator.cpu.x) land 0xFF in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      Emulator.read_word emu zero_page_addr
  | IndirectIndexed ->
      let zero_page_addr = Emulator.read_byte emu emu.Emulator.cpu.pc in
      emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
      let base_addr = Emulator.read_word emu zero_page_addr in
      (base_addr + emu.Emulator.cpu.y) land 0xFFFF
  | _ -> failwith "Invalid addressing mode for operand"

(* Basic instruction implementations *)

(* LDA - Load Accumulator *)
let lda emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  emu.Emulator.cpu.a <- value;
  Cpu.update_zn_flags emu.Emulator.cpu value

(* LDX - Load X Register *)
let ldx emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  emu.Emulator.cpu.x <- value;
  Cpu.update_zn_flags emu.Emulator.cpu value

(* LDY - Load Y Register *)
let ldy emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  emu.Emulator.cpu.y <- value;
  Cpu.update_zn_flags emu.Emulator.cpu value

(* STA - Store Accumulator *)
let sta emu mode =
  let addr = get_operand_address emu mode in
  Emulator.write_byte emu addr emu.Emulator.cpu.a

(* STX - Store X Register *)
let stx emu mode =
  let addr = get_operand_address emu mode in
  Emulator.write_byte emu addr emu.Emulator.cpu.x

(* STY - Store Y Register *)
let sty emu mode =
  let addr = get_operand_address emu mode in
  Emulator.write_byte emu addr emu.Emulator.cpu.y

(* INX - Increment X *)
let inx emu =
  emu.Emulator.cpu.x <- (emu.Emulator.cpu.x + 1) land 0xFF;
  Cpu.update_zn_flags emu.Emulator.cpu emu.Emulator.cpu.x

(* INY - Increment Y *)
let iny emu =
  emu.Emulator.cpu.y <- (emu.Emulator.cpu.y + 1) land 0xFF;
  Cpu.update_zn_flags emu.Emulator.cpu emu.Emulator.cpu.y

(* DEX - Decrement X *)
let dex emu =
  emu.Emulator.cpu.x <- (emu.Emulator.cpu.x - 1) land 0xFF;
  Cpu.update_zn_flags emu.Emulator.cpu emu.Emulator.cpu.x

(* DEY - Decrement Y *)
let dey emu =
  emu.Emulator.cpu.y <- (emu.Emulator.cpu.y - 1) land 0xFF;
  Cpu.update_zn_flags emu.Emulator.cpu emu.Emulator.cpu.y

(* NOP - No Operation *)
let nop _emu = ()

(* BRK - Break *)
let brk emu =
  emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;  (* BRK is 2 bytes *)
  Emulator.push_word emu emu.Emulator.cpu.pc;
  emu.Emulator.cpu.flags.break <- true;
  Emulator.push_byte emu (Cpu.flags_to_byte emu.Emulator.cpu.flags);
  emu.Emulator.cpu.flags.interrupt <- true;
  emu.Emulator.cpu.pc <- Emulator.read_word emu 0xFFFE  (* IRQ vector *)

(* JMP - Jump *)
let jmp emu mode =
  let addr = match mode with
    | Absolute ->
        Emulator.read_word emu emu.Emulator.cpu.pc
    | Indirect ->
        let indirect_addr = Emulator.read_word emu emu.Emulator.cpu.pc in
        Emulator.read_word emu indirect_addr
    | _ -> failwith "Invalid addressing mode for JMP"
  in
  emu.Emulator.cpu.pc <- addr

(* CMP - Compare with Accumulator *)
let cmp emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  let result = emu.Emulator.cpu.a - value in
  emu.Emulator.cpu.flags.carry <- emu.Emulator.cpu.a >= value;
  emu.Emulator.cpu.flags.zero <- (result = 0);
  emu.Emulator.cpu.flags.negative <- (result land 0x80) <> 0

(* BNE - Branch if Not Equal *)
let bne emu =
  let offset = Emulator.read_byte emu emu.Emulator.cpu.pc in
  emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
  if not emu.Emulator.cpu.flags.zero then (
    let signed_offset = if offset > 127 then offset - 256 else offset in
    emu.Emulator.cpu.pc <- (emu.Emulator.cpu.pc + signed_offset) land 0xFFFF
  )

(* BEQ - Branch if Equal *)
let beq emu =
  let offset = Emulator.read_byte emu emu.Emulator.cpu.pc in
  emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
  if emu.Emulator.cpu.flags.zero then (
    let signed_offset = if offset > 127 then offset - 256 else offset in
    emu.Emulator.cpu.pc <- (emu.Emulator.cpu.pc + signed_offset) land 0xFFFF
  )

(* ADC - Add with Carry *)
let adc emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  let carry = if emu.Emulator.cpu.flags.carry then 1 else 0 in
  let result = emu.Emulator.cpu.a + value + carry in
  
  (* Set flags *)
  emu.Emulator.cpu.flags.carry <- result > 255;
  emu.Emulator.cpu.flags.zero <- (result land 0xFF) = 0;
  emu.Emulator.cpu.flags.negative <- (result land 0x80) <> 0;
  
  (* Check for overflow *)
  let a_sign = emu.Emulator.cpu.a land 0x80 in
  let v_sign = value land 0x80 in
  let r_sign = result land 0x80 in
  emu.Emulator.cpu.flags.overflow <- (a_sign = v_sign) && (a_sign <> r_sign);
  
  emu.Emulator.cpu.a <- result land 0xFF

(* SBC - Subtract with Carry *)
let sbc emu mode =
  let addr = get_operand_address emu mode in
  let value = Emulator.read_byte emu addr in
  let carry = if emu.Emulator.cpu.flags.carry then 0 else 1 in
  let result = emu.Emulator.cpu.a - value - carry in
  
  (* Set flags *)
  emu.Emulator.cpu.flags.carry <- result >= 0;
  emu.Emulator.cpu.flags.zero <- (result land 0xFF) = 0;
  emu.Emulator.cpu.flags.negative <- (result land 0x80) <> 0;
  
  (* Check for overflow *)
  let a_sign = emu.Emulator.cpu.a land 0x80 in
  let v_sign = value land 0x80 in
  let r_sign = result land 0x80 in
  emu.Emulator.cpu.flags.overflow <- (a_sign <> v_sign) && (a_sign <> r_sign);
  
  emu.Emulator.cpu.a <- result land 0xFF

(* Create instruction table - we'll implement more as we go *)
let create_instruction_table () =
  let table = Array.make 256 None in
  
  (* LDA instructions *)
  table.(0xA9) <- Some { opcode = 0xA9; name = "LDA"; mode = Immediate; cycles = 2; execute = fun emu -> lda emu Immediate };
  table.(0xA5) <- Some { opcode = 0xA5; name = "LDA"; mode = ZeroPage; cycles = 3; execute = fun emu -> lda emu ZeroPage };
  table.(0xB5) <- Some { opcode = 0xB5; name = "LDA"; mode = ZeroPageX; cycles = 4; execute = fun emu -> lda emu ZeroPageX };
  table.(0xAD) <- Some { opcode = 0xAD; name = "LDA"; mode = Absolute; cycles = 4; execute = fun emu -> lda emu Absolute };
  table.(0xBD) <- Some { opcode = 0xBD; name = "LDA"; mode = AbsoluteX; cycles = 4; execute = fun emu -> lda emu AbsoluteX };
  table.(0xB9) <- Some { opcode = 0xB9; name = "LDA"; mode = AbsoluteY; cycles = 4; execute = fun emu -> lda emu AbsoluteY };
  table.(0xA1) <- Some { opcode = 0xA1; name = "LDA"; mode = IndexedIndirect; cycles = 6; execute = fun emu -> lda emu IndexedIndirect };
  table.(0xB1) <- Some { opcode = 0xB1; name = "LDA"; mode = IndirectIndexed; cycles = 5; execute = fun emu -> lda emu IndirectIndexed };
  
  (* STA instructions *)
  table.(0x85) <- Some { opcode = 0x85; name = "STA"; mode = ZeroPage; cycles = 3; execute = fun emu -> sta emu ZeroPage };
  table.(0x95) <- Some { opcode = 0x95; name = "STA"; mode = ZeroPageX; cycles = 4; execute = fun emu -> sta emu ZeroPageX };
  table.(0x8D) <- Some { opcode = 0x8D; name = "STA"; mode = Absolute; cycles = 4; execute = fun emu -> sta emu Absolute };
  table.(0x9D) <- Some { opcode = 0x9D; name = "STA"; mode = AbsoluteX; cycles = 5; execute = fun emu -> sta emu AbsoluteX };
  table.(0x99) <- Some { opcode = 0x99; name = "STA"; mode = AbsoluteY; cycles = 5; execute = fun emu -> sta emu AbsoluteY };
  table.(0x81) <- Some { opcode = 0x81; name = "STA"; mode = IndexedIndirect; cycles = 6; execute = fun emu -> sta emu IndexedIndirect };
  table.(0x91) <- Some { opcode = 0x91; name = "STA"; mode = IndirectIndexed; cycles = 6; execute = fun emu -> sta emu IndirectIndexed };
  
  (* JMP instructions *)
  table.(0x4C) <- Some { opcode = 0x4C; name = "JMP"; mode = Absolute; cycles = 3; execute = fun emu -> jmp emu Absolute };
  table.(0x6C) <- Some { opcode = 0x6C; name = "JMP"; mode = Indirect; cycles = 5; execute = fun emu -> jmp emu Indirect };
  
  (* CMP instructions *)
  table.(0xC9) <- Some { opcode = 0xC9; name = "CMP"; mode = Immediate; cycles = 2; execute = fun emu -> cmp emu Immediate };
  table.(0xC5) <- Some { opcode = 0xC5; name = "CMP"; mode = ZeroPage; cycles = 3; execute = fun emu -> cmp emu ZeroPage };
  table.(0xD5) <- Some { opcode = 0xD5; name = "CMP"; mode = ZeroPageX; cycles = 4; execute = fun emu -> cmp emu ZeroPageX };
  table.(0xCD) <- Some { opcode = 0xCD; name = "CMP"; mode = Absolute; cycles = 4; execute = fun emu -> cmp emu Absolute };
  table.(0xDD) <- Some { opcode = 0xDD; name = "CMP"; mode = AbsoluteX; cycles = 4; execute = fun emu -> cmp emu AbsoluteX };
  table.(0xD9) <- Some { opcode = 0xD9; name = "CMP"; mode = AbsoluteY; cycles = 4; execute = fun emu -> cmp emu AbsoluteY };
  table.(0xC1) <- Some { opcode = 0xC1; name = "CMP"; mode = IndexedIndirect; cycles = 6; execute = fun emu -> cmp emu IndexedIndirect };
  table.(0xD1) <- Some { opcode = 0xD1; name = "CMP"; mode = IndirectIndexed; cycles = 5; execute = fun emu -> cmp emu IndirectIndexed };
  
  (* BNE instructions *)
  table.(0xD0) <- Some { opcode = 0xD0; name = "BNE"; mode = Relative; cycles = 2; execute = fun emu -> bne emu };
  
  (* BEQ instructions *)
  table.(0xF0) <- Some { opcode = 0xF0; name = "BEQ"; mode = Relative; cycles = 2; execute = fun emu -> beq emu };
  
  (* ADC instructions *)
  table.(0x69) <- Some { opcode = 0x69; name = "ADC"; mode = Immediate; cycles = 2; execute = fun emu -> adc emu Immediate };
  table.(0x65) <- Some { opcode = 0x65; name = "ADC"; mode = ZeroPage; cycles = 3; execute = fun emu -> adc emu ZeroPage };
  table.(0x75) <- Some { opcode = 0x75; name = "ADC"; mode = ZeroPageX; cycles = 4; execute = fun emu -> adc emu ZeroPageX };
  table.(0x6D) <- Some { opcode = 0x6D; name = "ADC"; mode = Absolute; cycles = 4; execute = fun emu -> adc emu Absolute };
  table.(0x7D) <- Some { opcode = 0x7D; name = "ADC"; mode = AbsoluteX; cycles = 4; execute = fun emu -> adc emu AbsoluteX };
  table.(0x79) <- Some { opcode = 0x79; name = "ADC"; mode = AbsoluteY; cycles = 4; execute = fun emu -> adc emu AbsoluteY };
  table.(0x61) <- Some { opcode = 0x61; name = "ADC"; mode = IndexedIndirect; cycles = 6; execute = fun emu -> adc emu IndexedIndirect };
  table.(0x71) <- Some { opcode = 0x71; name = "ADC"; mode = IndirectIndexed; cycles = 5; execute = fun emu -> adc emu IndirectIndexed };
  
  (* SBC instructions *)
  table.(0xE9) <- Some { opcode = 0xE9; name = "SBC"; mode = Immediate; cycles = 2; execute = fun emu -> sbc emu Immediate };
  table.(0xE5) <- Some { opcode = 0xE5; name = "SBC"; mode = ZeroPage; cycles = 3; execute = fun emu -> sbc emu ZeroPage };
  table.(0xF5) <- Some { opcode = 0xF5; name = "SBC"; mode = ZeroPageX; cycles = 4; execute = fun emu -> sbc emu ZeroPageX };
  table.(0xED) <- Some { opcode = 0xED; name = "SBC"; mode = Absolute; cycles = 4; execute = fun emu -> sbc emu Absolute };
  table.(0xFD) <- Some { opcode = 0xFD; name = "SBC"; mode = AbsoluteX; cycles = 4; execute = fun emu -> sbc emu AbsoluteX };
  table.(0xF9) <- Some { opcode = 0xF9; name = "SBC"; mode = AbsoluteY; cycles = 4; execute = fun emu -> sbc emu AbsoluteY };
  table.(0xE1) <- Some { opcode = 0xE1; name = "SBC"; mode = IndexedIndirect; cycles = 6; execute = fun emu -> sbc emu IndexedIndirect };
  table.(0xF1) <- Some { opcode = 0xF1; name = "SBC"; mode = IndirectIndexed; cycles = 5; execute = fun emu -> sbc emu IndirectIndexed };
  
  (* Basic register operations *)
  table.(0xE8) <- Some { opcode = 0xE8; name = "INX"; mode = Implicit; cycles = 2; execute = fun emu -> inx emu };
  table.(0xC8) <- Some { opcode = 0xC8; name = "INY"; mode = Implicit; cycles = 2; execute = fun emu -> iny emu };
  table.(0xCA) <- Some { opcode = 0xCA; name = "DEX"; mode = Implicit; cycles = 2; execute = fun emu -> dex emu };
  table.(0x88) <- Some { opcode = 0x88; name = "DEY"; mode = Implicit; cycles = 2; execute = fun emu -> dey emu };
  
  (* System *)
  table.(0xEA) <- Some { opcode = 0xEA; name = "NOP"; mode = Implicit; cycles = 2; execute = fun emu -> nop emu };
  table.(0x00) <- Some { opcode = 0x00; name = "BRK"; mode = Implicit; cycles = 7; execute = fun emu -> brk emu };
  
  (* Jump instructions *)
  table.(0x4C) <- Some { opcode = 0x4C; name = "JMP"; mode = Absolute; cycles = 3; execute = fun emu -> jmp emu Absolute };
  table.(0x6C) <- Some { opcode = 0x6C; name = "JMP"; mode = Indirect; cycles = 5; execute = fun emu -> jmp emu Indirect };
  
  (* Compare instructions *)
  table.(0xC9) <- Some { opcode = 0xC9; name = "CMP"; mode = Immediate; cycles = 2; execute = fun emu -> cmp emu Immediate };
  table.(0xC5) <- Some { opcode = 0xC5; name = "CMP"; mode = ZeroPage; cycles = 3; execute = fun emu -> cmp emu ZeroPage };
  table.(0xCD) <- Some { opcode = 0xCD; name = "CMP"; mode = Absolute; cycles = 4; execute = fun emu -> cmp emu Absolute };
  
  (* Branch instructions *)
  table.(0xD0) <- Some { opcode = 0xD0; name = "BNE"; mode = Relative; cycles = 2; execute = fun emu -> bne emu };
  table.(0xF0) <- Some { opcode = 0xF0; name = "BEQ"; mode = Relative; cycles = 2; execute = fun emu -> beq emu };
  
  (* Arithmetic instructions *)
  table.(0x69) <- Some { opcode = 0x69; name = "ADC"; mode = Immediate; cycles = 2; execute = fun emu -> adc emu Immediate };
  table.(0x65) <- Some { opcode = 0x65; name = "ADC"; mode = ZeroPage; cycles = 3; execute = fun emu -> adc emu ZeroPage };
  table.(0x6D) <- Some { opcode = 0x6D; name = "ADC"; mode = Absolute; cycles = 4; execute = fun emu -> adc emu Absolute };
  
  table.(0xE9) <- Some { opcode = 0xE9; name = "SBC"; mode = Immediate; cycles = 2; execute = fun emu -> sbc emu Immediate };
  table.(0xE5) <- Some { opcode = 0xE5; name = "SBC"; mode = ZeroPage; cycles = 3; execute = fun emu -> sbc emu ZeroPage };
  table.(0xED) <- Some { opcode = 0xED; name = "SBC"; mode = Absolute; cycles = 4; execute = fun emu -> sbc emu Absolute };
  
  table

(* Execute one instruction *)
let step emu instruction_table =
  let opcode = Emulator.read_byte emu emu.Emulator.cpu.pc in
  emu.Emulator.cpu.pc <- emu.Emulator.cpu.pc + 1;
  
  match instruction_table.(opcode) with
  | Some instr ->
      Printf.printf "Executing: %s (opcode $%02X) at PC $%04X\n" instr.name opcode (emu.Emulator.cpu.pc - 1);
      instr.execute emu;
      instr.cycles
  | None ->
      Printf.printf "Unknown opcode: $%02X at PC $%04X\n" opcode (emu.Emulator.cpu.pc - 1);
      1  (* Return 1 cycle for unknown instructions *)
