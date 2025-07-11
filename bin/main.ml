(*
 * Main.ml - YANE (Yet Another NES Emulator)
 * 
 * This is the main entry point for our NES emulator.
 * For now, it's a console-based version to test the CPU.
 *)

open Yane

(* Test basic CPU functionality *)
let test_basic_cpu () =
  Printf.printf "=== Testing Basic CPU ===\n\n";
  
  (* Create emulator *)
  let emu = Emulator.create_emulator () in
  let instruction_table = Instructions.create_instruction_table () in
  
  (* Write a simple test program *)
  (* LDA #$42 - Load 0x42 into accumulator *)
  Emulator.write_byte emu 0x8000 0xA9;  (* LDA immediate *)
  Emulator.write_byte emu 0x8001 0x42;  (* Value 0x42 *)
  
  (* INX - Increment X *)
  Emulator.write_byte emu 0x8002 0xE8;  (* INX *)
  
  (* INY - Increment Y *)
  Emulator.write_byte emu 0x8003 0xC8;  (* INY *)
  
  (* STA $10 - Store accumulator to zero page *)
  Emulator.write_byte emu 0x8004 0x85;  (* STA zero page *)
  Emulator.write_byte emu 0x8005 0x10;  (* Address $10 *)
  
  (* BRK - Break *)
  Emulator.write_byte emu 0x8006 0x00;  (* BRK *)
  
  (* Power on first *)
  Emulator.power_on emu;
  
  (* Set reset vector to point to our program *)
  Emulator.write_word emu 0xFFFC 0x8000;  (* Reset vector points to $8000 *)
  
  (* Now reset to use the proper vector *)
  Emulator.reset emu;
  
  Printf.printf "Initial state:\n";
  Emulator.print_state emu;
  
  (* Execute instructions *)
  Printf.printf "\nExecuting test program...\n";
  for i = 1 to 5 do
    Printf.printf "\n--- Step %d ---\n" i;
    let _cycles = Instructions.step emu instruction_table in
    Emulator.print_state emu;
  done;
  
  Printf.printf "Memory at $10: $%02X\n" (Emulator.read_byte emu 0x10);
  Printf.printf "CPU test complete!\n\n"

(* Test ROM loading *)
let test_rom_loading () =
  Printf.printf "=== Testing ROM Loading ===\n\n";
  
  (* Check command line arguments *)
  match Sys.argv with
  | [| _; rom_file |] ->
      (try
         let rom = Rom.load_rom rom_file in
         Rom.print_rom_info rom;
         Printf.printf "ROM loaded successfully!\n\n";
         
         (* Create emulator and install ROM *)
         let emu = Emulator.create_emulator () in
         let instruction_table = Instructions.create_instruction_table () in
         Rom.install_rom emu rom;
         Emulator.power_on emu;
         
         Printf.printf "ROM installed. Running first few instructions...\n";
         for i = 1 to 10 do
           Printf.printf "\n--- Step %d ---\n" i;
           let _cycles = Instructions.step emu instruction_table in
           Emulator.print_state emu;
         done;
         
       with
       | Sys_error msg -> Printf.printf "Error loading ROM: %s\n" msg
       | Failure msg -> Printf.printf "Error: %s\n" msg
       | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
      )
  | _ -> Printf.printf "No ROM file specified. Use: %s <rom_file.nes>\n" Sys.argv.(0)

(* Main function *)
let main () =
  Printf.printf "=== YANE - Yet Another NES Emulator ===\n\n";
  
  (* Test basic CPU first *)
  test_basic_cpu ();
  
  (* Test ROM loading if ROM file provided *)
  test_rom_loading ();
  
  Printf.printf "\n=== Testing Complete ===\n";
  Printf.printf "This is a console-based test version.\n";
  Printf.printf "SDL graphics support will be added later.\n"

(* Entry point *)
let () = 
  try
    main ()
  with
  | e -> 
      Printf.printf "Fatal error: %s\n" (Printexc.to_string e);
      exit 1
