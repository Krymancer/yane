(*
 * Rom.ml - NES ROM loading and parsing
 * 
 * This module handles loading .nes ROM files and parsing their headers.
 *)

(* iNES header structure *)
type ines_header = {
  prg_rom_size : int;        (* PRG ROM size in 16KB units *)
  chr_rom_size : int;        (* CHR ROM size in 8KB units *)
  mapper : int;              (* Mapper number *)
  mirroring : int;           (* Mirroring type *)
  has_battery : bool;        (* Has battery-backed RAM *)
  has_trainer : bool;        (* Has trainer *)
  ignore_mirroring : bool;   (* Ignore mirroring control *)
}

(* NES ROM structure *)
type nes_rom = {
  header : ines_header;
  trainer : bytes option;    (* 512-byte trainer (if present) *)
  prg_rom : bytes;           (* PRG ROM data *)
  chr_rom : bytes;           (* CHR ROM data *)
}

(* Parse iNES header *)
let parse_ines_header data =
  if Bytes.length data < 16 then
    failwith "Invalid ROM: too short for iNES header"
  else
    let magic = Bytes.sub_string data 0 4 in
    if magic <> "NES\x1A" then
      failwith "Invalid ROM: not an iNES file"
    else
      let prg_size = Bytes.get_uint8 data 4 in
      let chr_size = Bytes.get_uint8 data 5 in
      let flags6 = Bytes.get_uint8 data 6 in
      let flags7 = Bytes.get_uint8 data 7 in
      
      let mapper = ((flags7 land 0xF0) lor (flags6 lsr 4)) in
      let mirroring = if (flags6 land 0x01) = 0 then 0 else 1 in
      let has_battery = (flags6 land 0x02) <> 0 in
      let has_trainer = (flags6 land 0x04) <> 0 in
      let ignore_mirroring = (flags6 land 0x08) <> 0 in
      
      {
        prg_rom_size = prg_size;
        chr_rom_size = chr_size;
        mapper = mapper;
        mirroring = mirroring;
        has_battery = has_battery;
        has_trainer = has_trainer;
        ignore_mirroring = ignore_mirroring;
      }

(* Load NES ROM from file *)
let load_rom filename =
  let channel = open_in_bin filename in
  let file_size = in_channel_length channel in
  let data = Bytes.create file_size in
  really_input channel data 0 file_size;
  close_in channel;
  
  let header = parse_ines_header data in
  let offset = ref 16 in
  
  (* Load trainer if present *)
  let trainer = 
    if header.has_trainer then (
      let trainer_data = Bytes.create 512 in
      Bytes.blit data !offset trainer_data 0 512;
      offset := !offset + 512;
      Some trainer_data
    ) else None
  in
  
  (* Load PRG ROM *)
  let prg_size = header.prg_rom_size * 16384 in (* 16KB units *)
  let prg_rom = Bytes.create prg_size in
  if prg_size > 0 then (
    Bytes.blit data !offset prg_rom 0 prg_size;
    offset := !offset + prg_size;
  );
  
  (* Load CHR ROM *)
  let chr_size = header.chr_rom_size * 8192 in (* 8KB units *)
  let chr_rom = Bytes.create chr_size in
  if chr_size > 0 then (
    Bytes.blit data !offset chr_rom 0 chr_size;
    offset := !offset + chr_size;
  );
  
  { header; trainer; prg_rom; chr_rom }

(* Install ROM into emulator memory *)
let install_rom emu rom =
  (* Install PRG ROM *)
  let prg_size = Bytes.length rom.prg_rom in
  if prg_size > 0 then (
    if prg_size = 16384 then (
      (* 16KB ROM - mirror it to both halves *)
      for i = 0 to 16383 do
        let value = Bytes.get_uint8 rom.prg_rom i in
        Emulator.write_byte emu (0x8000 + i) value;
        Emulator.write_byte emu (0xC000 + i) value;
      done
    ) else if prg_size = 32768 then (
      (* 32KB ROM - load normally *)
      for i = 0 to 32767 do
        let value = Bytes.get_uint8 rom.prg_rom i in
        Emulator.write_byte emu (0x8000 + i) value;
      done
    ) else
      Printf.printf "Warning: Unsupported PRG ROM size: %d bytes\n" prg_size
  );
  
  (* Install CHR ROM into PPU memory *)
  let chr_size = Bytes.length rom.chr_rom in
  if chr_size > 0 then (
    (* For now, we'll just print that we loaded it *)
    Printf.printf "Loaded CHR ROM: %d bytes\n" chr_size
  );
  
  Printf.printf "ROM installed: PRG=%dKB, CHR=%dKB, Mapper=%d\n" 
    (prg_size / 1024) (chr_size / 1024) rom.header.mapper

(* Print ROM information *)
let print_rom_info rom =
  Printf.printf "=== ROM Information ===\n";
  Printf.printf "PRG ROM: %d x 16KB = %dKB\n" 
    rom.header.prg_rom_size (rom.header.prg_rom_size * 16);
  Printf.printf "CHR ROM: %d x 8KB = %dKB\n" 
    rom.header.chr_rom_size (rom.header.chr_rom_size * 8);
  Printf.printf "Mapper: %d\n" rom.header.mapper;
  Printf.printf "Mirroring: %s\n" (if rom.header.mirroring = 0 then "Horizontal" else "Vertical");
  Printf.printf "Battery: %s\n" (if rom.header.has_battery then "Yes" else "No");
  Printf.printf "Trainer: %s\n" (if rom.header.has_trainer then "Yes" else "No");
  Printf.printf "========================\n"
