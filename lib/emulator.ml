(*
 * Emulator.ml - Main NES Emulator
 * 
 * This module ties together all the components:
 * CPU, Memory, PPU, APU, etc.
 *)

(* The main emulator state *)
type emulator = {
  cpu : Cpu.cpu_state;
  memory : Memory.memory;
  (* We'll add PPU, APU, etc. later *)
}

(* Create a new emulator *)
let create_emulator () = {
  cpu = Cpu.create_cpu ();
  memory = Memory.create_memory ();
}

(* CPU memory access functions *)
let read_byte emu address = Memory.read_byte emu.memory address
let write_byte emu address value = Memory.write_byte emu.memory address value
let read_word emu address = Memory.read_word emu.memory address
let write_word emu address value = Memory.write_word emu.memory address value

(* Stack operations *)
let push_byte emu value =
  Memory.push_byte emu.memory emu.cpu.sp value;
  emu.cpu.sp <- (emu.cpu.sp - 1) land 0xFF

let pop_byte emu =
  emu.cpu.sp <- (emu.cpu.sp + 1) land 0xFF;
  Memory.pop_byte emu.memory emu.cpu.sp

let push_word emu value =
  push_byte emu ((value lsr 8) land 0xFF);  (* High byte first *)
  push_byte emu (value land 0xFF)           (* Low byte second *)

let pop_word emu =
  let low = pop_byte emu in
  let high = pop_byte emu in
  low lor (high lsl 8)

(* Reset the emulator *)
let reset emu =
  (* Reset CPU *)
  emu.cpu.a <- 0;
  emu.cpu.x <- 0;
  emu.cpu.y <- 0;
  emu.cpu.sp <- 0xFD;
  emu.cpu.flags.interrupt <- true;
  emu.cpu.flags.unused <- true;
  
  (* Read reset vector from $FFFC-$FFFD *)
  let reset_vector = read_word emu 0xFFFC in
  emu.cpu.pc <- reset_vector;
  
  Printf.printf "Reset: PC set to $%04X\n" emu.cpu.pc

(* Power on the emulator *)
let power_on emu =
  (* Clear RAM *)
  for i = 0 to 0x07FF do
    write_byte emu i 0
  done;
  
  (* Don't set the reset vector here - let the program set it *)
  reset emu

(* Print emulator state *)
let print_state emu =
  Printf.printf "=== Emulator State ===\n";
  Cpu.print_cpu_state emu.cpu;
  Printf.printf "\n"
