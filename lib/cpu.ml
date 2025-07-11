(* 
 * CPU.ml - 6502 Processor Implementation
 * 
 * The 6502 is an 8-bit processor used in the NES.
 * It has:
 * - 8-bit accumulator (A)
 * - 8-bit index registers (X, Y)  
 * - 8-bit stack pointer (SP)
 * - 16-bit program counter (PC)
 * - 8-bit status register (P) with flags
 *)

(* In OCaml, we define types to represent our data structures *)
type status_flags = {
  mutable carry : bool;       (* C - Carry flag *)
  mutable zero : bool;        (* Z - Zero flag *)
  mutable interrupt : bool;   (* I - Interrupt disable *)
  mutable decimal : bool;     (* D - Decimal mode (unused in NES) *)
  mutable break : bool;       (* B - Break command *)
  mutable unused : bool;      (* - Unused flag *)
  mutable overflow : bool;    (* V - Overflow flag *)
  mutable negative : bool;    (* N - Negative flag *)
}

(* The CPU state - all registers and flags *)
type cpu_state = {
  mutable a : int;           (* Accumulator - 8 bits *)
  mutable x : int;           (* X index register - 8 bits *)
  mutable y : int;           (* Y index register - 8 bits *)
  mutable sp : int;          (* Stack pointer - 8 bits *)
  mutable pc : int;          (* Program counter - 16 bits *)
  flags : status_flags;      (* Processor status flags *)
}

(* Create a new CPU with initial state *)
let create_cpu () = {
  a = 0;
  x = 0;
  y = 0;
  sp = 0xFD;  (* Stack pointer starts at 0xFD *)
  pc = 0;     (* Program counter starts at 0 *)
  flags = {
    carry = false;
    zero = false;
    interrupt = true;   (* Start with interrupts disabled *)
    decimal = false;
    break = false;
    unused = true;      (* Always true *)
    overflow = false;
    negative = false;
  }
}

(* Helper functions to work with 8-bit values *)
let to_8bit value = value land 0xFF
let to_16bit value = value land 0xFFFF

(* Check if a value is negative (bit 7 set) *)
let is_negative value = (value land 0x80) <> 0

(* Update zero and negative flags based on a value *)
let update_zn_flags cpu value =
  cpu.flags.zero <- (value = 0);
  cpu.flags.negative <- is_negative value

(* Convert status flags to a single byte *)
let flags_to_byte flags =
  let result = ref 0 in
  if flags.carry then result := !result lor 0x01;
  if flags.zero then result := !result lor 0x02;
  if flags.interrupt then result := !result lor 0x04;
  if flags.decimal then result := !result lor 0x08;
  if flags.break then result := !result lor 0x10;
  if flags.unused then result := !result lor 0x20;
  if flags.overflow then result := !result lor 0x40;
  if flags.negative then result := !result lor 0x80;
  !result

(* Convert a byte to status flags *)
let byte_to_flags byte =
  {
    carry = (byte land 0x01) <> 0;
    zero = (byte land 0x02) <> 0;
    interrupt = (byte land 0x04) <> 0;
    decimal = (byte land 0x08) <> 0;
    break = (byte land 0x10) <> 0;
    unused = (byte land 0x20) <> 0;
    overflow = (byte land 0x40) <> 0;
    negative = (byte land 0x80) <> 0;
  }

(* Print CPU state for debugging *)
let print_cpu_state cpu =
  Printf.printf "CPU State:\n";
  Printf.printf "  A: $%02X  X: $%02X  Y: $%02X\n" cpu.a cpu.x cpu.y;
  Printf.printf "  PC: $%04X  SP: $%02X\n" cpu.pc cpu.sp;
  Printf.printf "  Flags: %s%s%s%s%s%s%s%s\n"
    (if cpu.flags.negative then "N" else "n")
    (if cpu.flags.overflow then "V" else "v")
    (if cpu.flags.unused then "U" else "u")
    (if cpu.flags.break then "B" else "b")
    (if cpu.flags.decimal then "D" else "d")
    (if cpu.flags.interrupt then "I" else "i")
    (if cpu.flags.zero then "Z" else "z")
    (if cpu.flags.carry then "C" else "c")
