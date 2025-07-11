(*
 * PPU.ml - NES Picture Processing Unit
 * 
 * The PPU handles graphics rendering for the NES.
 * It has its own memory space and generates the video signal.
 *)

(* PPU registers - these are mapped to CPU memory space $2000-$3FFF *)
type ppu_registers = {
  mutable ctrl : int;        (* $2000 - PPUCTRL *)
  mutable mask : int;        (* $2001 - PPUMASK *)
  mutable status : int;      (* $2002 - PPUSTATUS *)
  mutable oam_addr : int;    (* $2003 - OAMADDR *)
  mutable oam_data : int;    (* $2004 - OAMDATA *)
  mutable scroll : int;      (* $2005 - PPUSCROLL *)
  mutable addr : int;        (* $2006 - PPUADDR *)
  mutable data : int;        (* $2007 - PPUDATA *)
}

(* PPU memory layout *)
type ppu_memory = {
  pattern_table : bytes;     (* $0000-$1FFF - Pattern tables (8KB) *)
  name_table : bytes;        (* $2000-$3EFF - Name tables (4KB) *)
  palette : bytes;           (* $3F00-$3FFF - Palette (32 bytes) *)
}

(* PPU state *)
type ppu_state = {
  registers : ppu_registers;
  memory : ppu_memory;
  mutable cycle : int;       (* Current cycle *)
  mutable scanline : int;    (* Current scanline *)
  mutable frame : int;       (* Current frame *)
  mutable vblank : bool;     (* Vertical blank flag *)
}

(* Screen dimensions *)
let screen_width = 256
let screen_height = 240

(* Create new PPU *)
let create_ppu () = {
  registers = {
    ctrl = 0;
    mask = 0;
    status = 0;
    oam_addr = 0;
    oam_data = 0;
    scroll = 0;
    addr = 0;
    data = 0;
  };
  memory = {
    pattern_table = Bytes.create 0x2000;  (* 8KB *)
    name_table = Bytes.create 0x1000;     (* 4KB *)
    palette = Bytes.create 0x20;          (* 32 bytes *)
  };
  cycle = 0;
  scanline = 0;
  frame = 0;
  vblank = false;
}

(* Read from PPU register *)
let read_register ppu register =
  match register with
  | 0x2002 -> (* PPUSTATUS *)
      let status = ppu.registers.status in
      ppu.registers.status <- ppu.registers.status land 0x7F; (* Clear vblank flag *)
      status
  | 0x2004 -> (* OAMDATA *)
      ppu.registers.oam_data
  | 0x2007 -> (* PPUDATA *)
      ppu.registers.data
  | _ -> 0

(* Write to PPU register *)
let write_register ppu register value =
  match register with
  | 0x2000 -> (* PPUCTRL *)
      ppu.registers.ctrl <- value land 0xFF
  | 0x2001 -> (* PPUMASK *)
      ppu.registers.mask <- value land 0xFF
  | 0x2003 -> (* OAMADDR *)
      ppu.registers.oam_addr <- value land 0xFF
  | 0x2004 -> (* OAMDATA *)
      ppu.registers.oam_data <- value land 0xFF
  | 0x2005 -> (* PPUSCROLL *)
      ppu.registers.scroll <- value land 0xFF
  | 0x2006 -> (* PPUADDR *)
      ppu.registers.addr <- value land 0xFF
  | 0x2007 -> (* PPUDATA *)
      ppu.registers.data <- value land 0xFF
  | _ -> ()

(* Step PPU one cycle *)
let step ppu =
  ppu.cycle <- ppu.cycle + 1;
  
  (* Each scanline is 341 cycles *)
  if ppu.cycle >= 341 then (
    ppu.cycle <- 0;
    ppu.scanline <- ppu.scanline + 1;
    
    (* Check for vblank (scanline 241) *)
    if ppu.scanline = 241 then (
      ppu.vblank <- true;
      ppu.registers.status <- ppu.registers.status lor 0x80; (* Set vblank flag *)
    );
    
    (* End of frame (scanline 261) *)
    if ppu.scanline >= 262 then (
      ppu.scanline <- 0;
      ppu.frame <- ppu.frame + 1;
      ppu.vblank <- false;
      ppu.registers.status <- ppu.registers.status land 0x7F; (* Clear vblank flag *)
    );
  )

(* Simple color palette for now *)
let nes_palette = [|
  (0x80, 0x80, 0x80); (0x00, 0x3D, 0xA6); (0x00, 0x12, 0xB0); (0x44, 0x00, 0x96);
  (0xA1, 0x00, 0x5E); (0xC7, 0x00, 0x28); (0xBA, 0x06, 0x00); (0x8C, 0x17, 0x00);
  (0x5C, 0x2F, 0x00); (0x10, 0x45, 0x00); (0x05, 0x4A, 0x00); (0x00, 0x47, 0x2E);
  (0x00, 0x41, 0x66); (0x00, 0x00, 0x00); (0x05, 0x05, 0x05); (0x05, 0x05, 0x05);
  (* ... more colors would go here ... *)
|]

(* Render a simple test pattern *)
let render_test_pattern ppu framebuffer =
  for y = 0 to screen_height - 1 do
    for x = 0 to screen_width - 1 do
      let color_index = (x + y) / 8 mod 4 in
      let (r, g, b) = nes_palette.(color_index) in
      let pixel_index = (y * screen_width + x) * 4 in
      Bytes.set_uint8 framebuffer pixel_index r;
      Bytes.set_uint8 framebuffer (pixel_index + 1) g;
      Bytes.set_uint8 framebuffer (pixel_index + 2) b;
      Bytes.set_uint8 framebuffer (pixel_index + 3) 255; (* Alpha *)
    done
  done

(* Print PPU state for debugging *)
let print_ppu_state ppu =
  Printf.printf "PPU State:\n";
  Printf.printf "  Cycle: %d  Scanline: %d  Frame: %d\n" ppu.cycle ppu.scanline ppu.frame;
  Printf.printf "  VBlank: %s\n" (if ppu.vblank then "Yes" else "No");
  Printf.printf "  CTRL: $%02X  MASK: $%02X  STATUS: $%02X\n" 
    ppu.registers.ctrl ppu.registers.mask ppu.registers.status
