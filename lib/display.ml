(*
 * Display.ml - SDL-based display for the NES emulator
 * 
 * This module handles the SDL window and rendering.
 *)

open Tsdl

(* Display constants *)
let screen_width = 256
let screen_height = 240
let scale_factor = 3

(* Display state *)
type display = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  texture : Sdl.texture;
  framebuffer : bytes;
}

(* Initialize SDL and create display *)
let create_display () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> failwith ("SDL init failed: " ^ e)
  | Ok () ->
      let window_width = screen_width * scale_factor in
      let window_height = screen_height * scale_factor in
      
      match Sdl.create_window ~w:window_width ~h:window_height
        "YANE - Yet Another NES Emulator" Sdl.Window.shown with
      | Error (`Msg e) -> failwith ("SDL window creation failed: " ^ e)
      | Ok window ->
          match Sdl.create_renderer window ~index:(-1) Sdl.Renderer.accelerated with
          | Error (`Msg e) -> failwith ("SDL renderer creation failed: " ^ e)
          | Ok renderer ->
              match Sdl.create_texture renderer Sdl.Pixel.format_rgba8888 
                      Sdl.Texture.access_streaming ~w:screen_width ~h:screen_height with
              | Error (`Msg e) -> failwith ("SDL texture creation failed: " ^ e)
              | Ok texture ->
                  let framebuffer = Bytes.create (screen_width * screen_height * 4) in
                  { window; renderer; texture; framebuffer }

(* Update display with new frame *)
let update_display display =
  (* Update texture with framebuffer *)
  let pitch = screen_width * 4 in
  (match Sdl.update_texture display.texture None display.framebuffer pitch with
   | Error (`Msg e) -> Printf.printf "Error updating texture: %s\n" e
   | Ok () -> ()
  );
  
  (* Clear and render *)
  (match Sdl.render_clear display.renderer with
   | Error (`Msg e) -> Printf.printf "Error clearing renderer: %s\n" e
   | Ok () -> ()
  );
  
  (match Sdl.render_copy display.renderer display.texture with
   | Error (`Msg e) -> Printf.printf "Error copying texture: %s\n" e
   | Ok () -> ()
  );
  
  Sdl.render_present display.renderer

(* Handle SDL events *)
let handle_events () =
  let event = Sdl.Event.create () in
  let rec loop quit =
    if Sdl.poll_event (Some event) then
      match Sdl.Event.(enum (get event typ)) with
      | `Quit -> true
      | `Key_down ->
          let key = Sdl.Event.(get event keyboard_scancode) in
          if key = Sdl.Scancode.escape then true else loop quit
      | _ -> loop quit
    else quit
  in
  loop false

(* Clean up SDL resources *)
let cleanup_display display =
  Sdl.destroy_texture display.texture;
  Sdl.destroy_renderer display.renderer;
  Sdl.destroy_window display.window;
  Sdl.quit ()

(* Fill framebuffer with test pattern *)
let fill_test_pattern display =
  for y = 0 to screen_height - 1 do
    for x = 0 to screen_width - 1 do
      let color_index = (x + y) / 8 mod 4 in
      let (r, g, b) = match color_index with
        | 0 -> (0, 0, 0)       (* Black *)
        | 1 -> (255, 0, 0)     (* Red *)
        | 2 -> (0, 255, 0)     (* Green *)
        | 3 -> (0, 0, 255)     (* Blue *)
        | _ -> (255, 255, 255) (* White *)
      in
      let pixel_index = (y * screen_width + x) * 4 in
      Bytes.set_uint8 display.framebuffer pixel_index r;
      Bytes.set_uint8 display.framebuffer (pixel_index + 1) g;
      Bytes.set_uint8 display.framebuffer (pixel_index + 2) b;
      Bytes.set_uint8 display.framebuffer (pixel_index + 3) 255; (* Alpha *)
    done
  done
