(*
 * Memory.ml - NES Memory Management
 * 
 * The NES has a specific memory layout:
 * $0000-$07FF: RAM (2KB, mirrored 4 times to fill $0000-$1FFF)
 * $2000-$3FFF: PPU registers (mirrored every 8 bytes)
 * $4000-$4017: APU and I/O registers
 * $4018-$401F: APU and I/O test functionality
 * $4020-$FFFF: Cartridge space (PRG ROM, PRG RAM, mapper registers)
 *)

(* Memory layout constants *)
let ram_size = 0x800        (* 2KB internal RAM *)
let ram_start = 0x0000
let ram_end = 0x1FFF        (* Mirrored 4 times *)

let ppu_start = 0x2000
let ppu_end = 0x3FFF

let apu_io_start = 0x4000
let apu_io_end = 0x4017

let cartridge_start = 0x4020
let cartridge_end = 0xFFFF

(* Memory state *)
type memory = {
  ram : bytes;              (* 2KB internal RAM *)
  cartridge : bytes;        (* Simple cartridge space for testing *)
}

(* Create new memory *)
let create_memory () = {
  ram = Bytes.create ram_size;
  cartridge = Bytes.create (0x10000 - cartridge_start);  (* Full remaining address space *)
}

(* Read a byte from memory *)
let read_byte memory address =
  let addr = address land 0xFFFF in  (* Ensure 16-bit address *)
  match addr with
  | a when a >= ram_start && a <= ram_end ->
      (* RAM is mirrored 4 times, so we use modulo to get actual address *)
      let ram_addr = a mod ram_size in
      Bytes.get_uint8 memory.ram ram_addr
  | a when a >= ppu_start && a <= ppu_end ->
      (* PPU registers - not implemented yet *)
      0
  | a when a >= apu_io_start && a <= apu_io_end ->
      (* APU/IO registers - not implemented yet *)
      0
  | a when a >= cartridge_start ->
      (* Cartridge space *)
      let cart_addr = a - cartridge_start in
      if cart_addr < Bytes.length memory.cartridge then
        Bytes.get_uint8 memory.cartridge cart_addr
      else
        0
  | _ -> 0

(* Write a byte to memory *)
let write_byte memory address value =
  let addr = address land 0xFFFF in
  let val8 = value land 0xFF in
  match addr with
  | a when a >= ram_start && a <= ram_end ->
      let ram_addr = a mod ram_size in
      Bytes.set_uint8 memory.ram ram_addr val8
  | a when a >= ppu_start && a <= ppu_end ->
      (* PPU registers - not implemented yet *)
      ()
  | a when a >= apu_io_start && a <= apu_io_end ->
      (* APU/IO registers - not implemented yet *)
      ()
  | a when a >= cartridge_start ->
      (* Cartridge space *)
      let cart_addr = a - cartridge_start in
      if cart_addr < Bytes.length memory.cartridge then
        Bytes.set_uint8 memory.cartridge cart_addr val8
  | _ -> ()

(* Read a 16-bit word (little-endian) *)
let read_word memory address =
  let low = read_byte memory address in
  let high = read_byte memory (address + 1) in
  low lor (high lsl 8)

(* Write a 16-bit word (little-endian) *)
let write_word memory address value =
  let low = value land 0xFF in
  let high = (value lsr 8) land 0xFF in
  write_byte memory address low;
  write_byte memory (address + 1) high

(* Stack operations - stack grows downward from $01FF *)
let stack_base = 0x0100

let push_byte memory sp value =
  let addr = stack_base lor sp in
  write_byte memory addr value

let pop_byte memory sp =
  let addr = stack_base lor sp in
  read_byte memory addr

(* Debug: print memory contents *)
let print_memory_range memory start_addr end_addr =
  Printf.printf "Memory from $%04X to $%04X:\n" start_addr end_addr;
  for addr = start_addr to end_addr do
    if addr mod 16 = 0 then Printf.printf "%04X: " addr;
    Printf.printf "%02X " (read_byte memory addr);
    if addr mod 16 = 15 then Printf.printf "\n";
  done;
  if (end_addr - start_addr + 1) mod 16 <> 0 then Printf.printf "\n"
