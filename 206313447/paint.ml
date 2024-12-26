(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)


(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. 

I added the Points and Ellipse shapes to handle those as well as 
thickness as a constructor to each one *)

type shape = 
  | Line of {color: color; p1: point; p2: point; t: thickness}
  | Points of { color: Gctx.color; points: point list; t: thickness}
  | Ellipse of {color : Gctx.color; p1: point; p2: point; t: thickness}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. 

I added Ellipse Start/ End mode as well as a mode for when we are supposed to 
draw points *)

type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point


(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 
  
  I added the thickness and prev_shape as a preview shape option*) 

  mutable thickness : thickness;

  mutable prev_shape : shape option;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  thickness = thin;
  prev_shape = None;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 
  I had the thickness stay as thin and the preview shape be initially empty*)
  
}


(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: Your will need to modify this function in Task 5 *)

(*I had to add the thickness as a parameter and to the function as a part of g*)
let with_params (g: gctx) (c: color) (t: thickness): gctx =
  let g1 = with_thickness g t in
  let g2 = with_color g1 c in
  g2


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
    (*I added the Points function to handle drawing points as well as the 
    Ellipse that essentially mimmicks the draw_line functions 
    I made sure in the Ellipse to calculate where the center and radii should 
    be so that it looks as specified*)
      | Line l -> draw_line (with_params g l.color l.t) l.p1 l.p2
      | Points ps -> Gctx.draw_points (with_params g ps.color ps.t) ps.points
      | Ellipse e -> 
      let ep: position = 
      ((fst e.p1)+(fst e.p2))/2, ((snd e.p1)+(snd e.p2))/2 in
      let rx : int = abs((fst e.p1 - fst e.p2)/2) in 
      let ry : int = abs((snd e.p1 - snd e.p2)/2)in 
      draw_ellipse (with_params g e.color e.t) ep rx ry
    end in
    let draw_preview (s: shape option) : unit =
    (*I added a function that draws previewed shapes so that it can specially
    handle drawing the shape options because the function above would oviously
    not accept the shape options as constructors *)
    begin match s with 
      |None -> ()
      |Some Line l -> draw_line (with_params g l.color l.t) l.p1 l.p2;
      |Some Points ps -> ()
      |Some Ellipse e -> 
      let ep: position = 
      ((fst e.p1)+(fst e.p2))/ 2, ((snd e.p1)+(snd e.p2))/2 in
      let rx : int = abs((fst e.p1 - fst e.p2)/2) in 
      let ry : int = abs((snd e.p1 - snd e.p2)/2)in 
      draw_ellipse (with_params g e.color e.t) ep rx ry
      end in
      (*this iterates through all of the shapes drawing each of them,
      while the draw_preview only draws the shape that exists in the 
      preview type (which goes away every time another event happens)*)
  Deque.iterate draw_shape paint.shapes;
  draw_preview paint.prev_shape


(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), 
(paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)

(*this is essentially a helper function so that I can get the points and use 
them as non options when I put them into the deque *)

let list_from_prev (s: shape option) : point list =
begin match s with 
|None -> []
|Some Line l -> []
|Some Points p -> p.points
|Some Ellipse e -> []
end

let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. It shifts the paint's mode into being in a "end mode"*)
            paint.mode <- LineEndMode p
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. Nothing happens.*)
            ()
          |PointMode -> let p1 = Points {color = paint.color; points = [p];
           t = paint.thickness} in 
          paint.prev_shape <- Some p1
          (*starts constructing the points and putting them in the preview*)
          |EllipseStartMode ->
          paint.mode <- EllipseEndMode p
          (*mimmicks the line mode changes*)
          |EllipseEndMode p-> 
          ()
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)

         (*puts each shape into the preview state while still dragging if the 
         mouse was already down and the user as a dragged click*)
         (begin match paint.mode with 
         |LineEndMode p1 -> let l1 = Line {color=paint.color; p1=p1; p2=p;  
         t = paint.thickness} in 
         paint.prev_shape <- Some l1
         |LineStartMode -> ()
         |PointMode -> let l1: point list = 
         (list_from_prev paint.prev_shape) @ [p] in 
         let p1 = Points {color = paint.color; points = l1;  
         t = paint.thickness} in 
         paint.prev_shape <- Some p1
         |EllipseEndMode p1 -> let e1 = 
         Ellipse {color=paint.color; p1 = p1; p2 = p;  t = paint.thickness} in 
         paint.prev_shape <- Some e1
         |EllipseStartMode -> ()
         end)
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2,
         3, 4, and possibly 6 need to do something different here. *)

         (*When the mouse gets released, all of the shapes stored in the 
         preview become stored in the permanent deque for shapes*)
      begin match paint.mode with
      |LineEndMode p1 ->
      Deque.insert_tail(Line {color=paint.color; p1=p1; p2=p;  
      t = paint.thickness}) paint.shapes;
      paint.prev_shape <- None;
      paint.mode <- LineStartMode
      |LineStartMode -> ()
      |PointMode -> let l : point list = list_from_prev paint.prev_shape in
      Deque.insert_tail(Points {color = paint.color; points = l;  
      t = paint.thickness}) paint.shapes;
      |EllipseEndMode p1 -> Deque.insert_tail(Ellipse {color=paint.color; 
      p1 = p1; p2 = p;  t = paint.thickness}) paint.shapes;
      paint.prev_shape <- None;
      paint.mode <- EllipseStartMode
      |EllipseStartMode -> ()
      end
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo button *)


let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially 2
   (depending on your implementation). *)

let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)


(*below I created the buttons so that when you press them it will change
 the mode based on whether you want to be drawing a line, an ellipse, 
 or points*)

let (w_line, lc_line, nc_line) = button "Line"

let liner () : unit =
  paint.mode <- LineStartMode

;; nc_line.add_event_listener (mouseclick_listener liner)

(*This creates a checkbox that can make the line become thicker*)

let mk_listener_thicker () : widget =
  let (switch_w, switch_cb) =
    checkbox false "Thickness" in
    let (w :widget), (nc: notifier_controller) = notifier switch_w in 
nc.add_event_listener (mouseclick_listener (fun () ->
  if paint.thickness = thick then (paint.thickness <- thin;) 
  else paint.thickness <- thick););
w

(*below are my three sliders that change  each of the three components
of color  so that you can completely change the color with the red, green,
and blue sliders*)

let sliding_r () : widget =
let (switch_w, switch_cb) =
    slider "Red" Gctx.red in
let (w :widget), (nc: notifier_controller) = notifier switch_w in
nc.add_event_listener (mouseclick_listener (fun () ->
  paint.color <- {r = (switch_cb.get_value () * 3);
                b = paint.color.b;
                g = paint.color.g}););
w

let sliding_g () : widget =
let (switch_w, switch_cb) =
    slider "Green" Gctx.green in
let (w :widget), (nc: notifier_controller) = notifier switch_w in
nc.add_event_listener (mouseclick_listener (fun () ->
  paint.color <- {r = paint.color.r;
                b = paint.color.b;
                g = (switch_cb.get_value () * 3)}););
w

let sliding_b () : widget =
let (switch_w, switch_cb) =
    slider "Blue" Gctx.blue in
let (w :widget), (nc: notifier_controller) = notifier switch_w in
nc.add_event_listener (mouseclick_listener (fun () ->
  paint.color <- {r = paint.color.r;
                b = (switch_cb.get_value () * 3);
                g = paint.color.g}););
w

let sliders () : widget = 
vlist [sliding_r (); sliding_g (); sliding_b ()]


let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

let ellipser () : unit =
  paint.mode <- EllipseStartMode

;; nc_ellipse.add_event_listener (mouseclick_listener ellipser)

let (w_point, lc_point, nc_point) = button "Points"

let pointer () : unit =
  paint.mode <- PointMode

;; nc_point.add_event_listener (mouseclick_listener pointer)

(** A spacer widget *)
let spacer : widget = space (10,10)

(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons
    to the toolbar in Task 1, Tasks 5, and possibly 6. *)
let mode_toolbar : widget = hlist 
[border(mk_listener_thicker ()); spacer; border w_undo; spacer; border w_point; 
spacer; border w_line; spacer; border w_ellipse]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator : widget=
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)
   let color_toolbar : widget =
   let l1 : widget list = [spacer; color_button black; spacer;
   color_button white; spacer; color_button red; spacer; color_button green; 
   spacer; color_button blue; spacer; color_button yellow; spacer; 
   color_button cyan; spacer ;color_button magenta; spacer; color_indicator] in 
   Widget.hlist l1
   

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)

(*this visually constructs the screen*)

let paint_widget=
  Widget.vlist [paint_canvas; spacer; 
  hpair(vlist [mode_toolbar; spacer; color_toolbar;]) (sliders ())]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
