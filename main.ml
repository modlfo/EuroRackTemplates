
open Gg
open Vg

let hp_table =
   [
      2, 9.8;
      4, 20.0;
      6, 30.0;
      8, 40.30;
      10, 50.50;
      12, 60.60;
      14, 70.80;
      16, 80.90;
      18, 91.30;
      20, 101.30;
      22, 111.40;
      28, 141.90;
      42, 213.0;
   ]


let default_width = 0.254 (* 10 mil *)

let fitToGrid x = (floor (x /. default_width)) *. default_width


let blendAll imgs =
   match imgs with
   | [] -> I.const Color.void
   | h :: t -> List.fold_left I.blend h t

let drawLine x1 y1 (x2:float) (y2:float) color =
   let white = I.const color in
   let area       = `O { P.o with P.width = default_width } in
   let line = P.empty |>  P.sub (P2.v x1 y1) |> P.line (P2.v x2 y2) in
   I.cut ~area line white

let drawCircle x y diameter color =
   let white = I.const color in
   let area       = `O { P.o with P.width = default_width } in
   let circle     = P.empty |> P.circle (P2.v x y) (diameter /. 2.0) in
   I.cut ~area circle white

let drawRect x y w h color =
   let white  = I.const color in
   let area   = `O { P.o with P.width = default_width } in
   let circle = P.empty |> P.rect (Box2.v (P2.v x y) (Size2.v w h)) in
   I.cut ~area circle white

let placeScrewHole x y diameter color =
   let white  = I.const color in
   let rad    = diameter /. 2.0 in
   let area   = `O { P.o with P.width = default_width } in
   let wide   = 0.8 in
   let line   =
      P.empty
      |> P.sub (P2.v (x +. wide) (y +. rad))
      |> P.earc ~cw:true (Size2.v rad rad) (P2.v (x +. wide) (y -. rad))
      |> P.line (P2.v (x -. wide) (y -. rad))
      |> P.earc ~cw:true (Size2.v rad rad) (P2.v (x -. wide) (y +. rad))
      |> P.close
   in
   I.cut ~area line white

let placeHole x y =
   let circle = drawCircle x y 3.2 Color.white in
   let lineH  = drawLine x (y -. 3.2/.2.0) x (y +. 3.2/.2.0) Color.white in
   let lineV  = drawLine (x -. 3.2/.2.0) y (x +. 3.2/.2.0) y Color.white in
   let screw  = I.blend lineH circle |> I.blend lineV in
   let hole   = placeScrewHole x y 3.2 Color.green in
   I.blend hole screw

let placeHoles hp =
   let left =
      let h1 = placeHole 7.5 3.0 in
      let h2 = placeHole 7.5 125.5 in
      Vg.I.blend h2 h1
   in
   let right =
      let hpr = float_of_int hp -. 3.0 in
      if hp > 4 then
         let h1 = placeHole (7.5 +. hpr *. 5.08) 3.0 in
         let h2 = placeHole (7.5 +. hpr *. 5.08) 125.5 in
         Vg.I.blend h2 h1
      else
         I.const Color.void
   in
   Vg.I.blend right left

let generateVGrid size actual_width actual_height =
   let half = actual_width /. 2.0 in
   let color = Color.gray 0.2 in
   let rec loop n img =
      let d = n *. size in
      if d < half then
         let line1 = drawLine (half -. d) 0.0 (half -. d) actual_height color in
         let line2 = drawLine (half +. d) 0.0 (half +. d) actual_height color in
         loop (n +. 1.0) (blendAll [img; line2; line1])
      else img
   in
   let center = drawLine half 0.0 half actual_height (Color.gray 0.5) in
   loop 1.0 center

let generateHGrid size actual_width actual_height =
   let half = actual_height /. 2.0 in
   let color = Color.gray 0.2 in
   let rec loop n img =
      let d = n *. size in
      if d < half then
         let line1 = drawLine 0.0 (half -. d) actual_width (half -. d)  color in
         let line2 = drawLine 0.0 (half +. d) actual_width (half +. d)  color in
         loop (n +. 1.0) (blendAll [img; line2; line1])
      else img
   in
   let center = drawLine 0.0 half actual_width half (Color.gray 0.5) in
   loop 1.0 center

let generateGrid size actual_width actual_height =
   let v = generateVGrid size actual_width actual_height in
   let h = generateHGrid size actual_width actual_height in
   I.blend v h

let generateRails actual_width actual_height =
   let line1 = drawLine 0.0 (actual_height -. 9.0) actual_width (actual_height -. 9.0) (Color.gray 0.5) in
   let line2 = drawLine 0.0 9.0 actual_width 9.0 (Color.gray 0.5) in
   I.blend line1 line2

let generateBorder actual_width actual_height =
   drawRect 0.0 0.0 actual_width actual_height (Color.red)

let generatePCB actual_width actual_height =
   let pcb_w = fitToGrid actual_width in
   let pcb_h = fitToGrid 100.0 in
   let y = (actual_height -. pcb_h) /. 2.0 in
   let x = (actual_width -. pcb_w) /. 2.0 in
   drawRect x y pcb_w pcb_h (Color.green)

let generatePanel (hp, width) =
   let actual_height = fitToGrid 128.5 in
   let actual_width = fitToGrid width in
   let size = Size2.v actual_width actual_height in
   let view = Box2.v P2.o (Size2.v actual_width actual_height) in
   let background = I.const Color.black in

   let holes = placeHoles hp in
   let grid = generateGrid 2.54 actual_width actual_height in
   let rails = generateRails actual_width actual_height in
   let border = generateBorder actual_width actual_height in
   let pcb = generatePCB actual_width actual_height in

   let image = blendAll [holes; rails; pcb; border; grid; background] in

   (* Render *)
   let hp_str = string_of_int hp in
   let title = "Eurorack template for " ^ hp_str ^ " HP" in
   let xmp = Vgr.xmp ~title () in
   let warn w = Vgr.pp_warning Format.err_formatter w in
   let oc = open_out ("templates/Back-" ^hp_str ^ "HP.svg") in
   let r = Vgr.create ~warn (Vgr_svg.target ~xmp ()) (`Channel oc) in
   ignore (Vgr.render r (`Image (size, view, image)));
   ignore (Vgr.render r `End);
   close_out oc

let main () =
   List.iter generatePanel hp_table
;;

main ()
