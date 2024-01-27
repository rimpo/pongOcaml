[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-69"]
[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-34"]

let pedal_width = 10
let pedal_height = 60

type position =
  { mutable x : int
  ; mutable y : int
  }

type vector =
  { mutable x : int
  ; mutable y : int
  }

type pedal =
  { pos : position
  ; width : int
  ; height : int
  ; color : Raylib.Color.t
  }

type player = { pedal : pedal }

type ball =
  { mutable pos : position
  ; radius : int
  ; vec : vector
  }

type board =
  { pos : position
  ; width : int
  ; height : int
  }

type game_state =
  | GameOver
  | GameRunning
  | GamePause

type game =
  { left_player : player
  ; right_player : player
  ; ball : ball
  ; board : board
  ; mutable state : game_state
  }

(* player pedal should not go outside the board *)
let force_pedal_inside_boundary (pedal : pedal) (board : board) =
  if pedal.pos.y < board.pos.y then pedal.pos.y <- board.pos.y;
  if pedal.pos.y + pedal.height > board.pos.y + board.height
  then pedal.pos.y <- board.pos.y + board.height - pedal.height
;;

(* ball should bounce on the top and bottom line of the board *)
let bounce_ball_on_boundary_y (ball : ball) (board : board) =
  (* ball crossed above top *)
  if ball.pos.y - ball.radius + ball.vec.y <= board.pos.y
  then (
    ball.pos.y <- board.pos.y + ball.radius;
    ball.vec.y <- -1 * ball.vec.y);
  (* ball crossed above bottom *)
  if ball.pos.y + ball.radius + ball.vec.y >= board.pos.y + board.height
  then (
    ball.pos.y <- board.pos.y + board.height - ball.radius;
    ball.vec.y <- -1 * ball.vec.y)
;;

(* check if the point is going to be inside the pedal *)
let point_inside_rectangle (pos : position) (pedal : pedal) =
  pos.x >= pedal.pos.x
  && pos.x <= pedal.pos.x + pedal.width
  && pos.y >= pedal.pos.y
  && pos.y <= pedal.pos.y + pedal.height
;;

(* bounce the ball on pedal if next position hitting the pedal *)
let bounce_ball_if_hitting_pedal game =
  let ball = game.ball in
  let left_pedal = game.left_player.pedal in
  let right_pedal = game.right_player.pedal in
  if point_inside_rectangle
       { x = ball.pos.x - ball.radius + ball.vec.x; y = ball.pos.y + ball.vec.y }
       left_pedal
  then (
    (* ball hit left pedal*)
    ball.pos.x <- left_pedal.pos.x + left_pedal.width + ball.radius;
    ball.vec.x <- -1 * ball.vec.x);
  if point_inside_rectangle
       { x = ball.pos.x + ball.radius + ball.vec.x; y = ball.pos.y + ball.vec.y }
       right_pedal
  then (
    (* ball hit right pedal*)
    ball.pos.x <- right_pedal.pos.x - ball.radius;
    ball.vec.x <- -1 * ball.vec.x)
;;

(* ball should bounce on the right and left for checking Gameover *)
let bounce_ball_on_boundary_x (ball : ball) (game : game) =
  let board = game.board in
  (* ball crossed board at right *)
  if ball.pos.x + ball.radius + ball.vec.x >= board.pos.x + board.width
  then (
    ball.pos.x <- board.pos.x + board.width - ball.radius;
    ball.vec.x <- -1 * ball.vec.x;
    game.state <- GameOver);
  (* ball crossed board at left *)
  if ball.pos.x - ball.radius + ball.vec.x <= board.pos.x
  then (
    ball.pos.x <- board.pos.x + ball.radius;
    ball.vec.x <- -1 * ball.vec.x;
    game.state <- GameOver)
;;

(* move ball and sure bouncing and game over conditions *)
let move_ball (ball : ball) (game : game) =
  ball.pos.x <- ball.pos.x + ball.vec.x;
  ball.pos.y <- ball.pos.y + ball.vec.y;
  bounce_ball_on_boundary_y ball game.board;
  bounce_ball_on_boundary_x ball game;
  bounce_ball_if_hitting_pedal game
;;

let move_player player board key_up key_down =
  let open Raylib in
  if is_key_down key_down then player.pedal.pos.y <- player.pedal.pos.y + 5;
  if is_key_down key_up then player.pedal.pos.y <- player.pedal.pos.y - 5;
  force_pedal_inside_boundary player.pedal board
;;

let reset_ball (p : position) w h : position = { x = p.x + (w / 2); y = p.y + (h / 2) }

let make_game (p : position) w h =
  { left_player =
      { pedal =
          { pos = { x = p.x + 20; y = 150 }
          ; height = pedal_height
          ; width = pedal_width
          ; color = Raylib.Color.green
          }
      }
  ; right_player =
      { pedal =
          { pos = { x = p.x + w - pedal_width - 20; y = 150 }
          ; height = pedal_height
          ; width = pedal_width
          ; color = Raylib.Color.blue
          }
      }
  ; ball = { pos = reset_ball p w h; radius = 10; vec = { x = 2; y = 2 } }
  ; board = { pos = p; width = w; height = h }
  ; state = GameRunning
  }
;;

let draw_ball (ball : ball) =
  let open Raylib in
  draw_circle ball.pos.x ball.pos.y (float_of_int ball.radius) Color.red
;;

let draw_pedal (pedal : pedal) =
  let open Raylib in
  draw_rectangle pedal.pos.x pedal.pos.y pedal.width pedal.height pedal.color
;;

let draw_board (board : board) color =
  let open Raylib in
  draw_rectangle_lines board.pos.x board.pos.y board.width board.height color;
  draw_line
    (board.pos.x + (board.width / 2))
    board.pos.y
    (board.pos.x + (board.width / 2))
    (board.pos.y + board.height)
    color;
  draw_circle_lines
    (board.pos.x + (board.width / 2))
    (board.pos.y + (board.height / 2))
    100.0
    color
;;

let draw_game game =
  draw_board game.board Raylib.Color.gray;
  draw_pedal game.left_player.pedal;
  draw_pedal game.right_player.pedal;
  draw_ball game.ball
;;

let setup w h =
  Raylib.init_window w h "pong";
  Raylib.set_target_fps 60;
  make_game { x = 10; y = 10 } (w - 20) (h - 20)
;;

let rec loop game =
  if Raylib.window_should_close ()
  then Raylib.close_window ()
  else
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    (match game.state with
     | GameRunning ->
       move_player game.left_player game.board Key.Q Key.A;
       move_player game.right_player game.board Key.Up Key.Down;
       move_ball game.ball game;
       draw_game game;
       if is_key_pressed Key.Space then game.state <- GamePause
     | GameOver ->
       draw_game game;
       draw_text "Game Over!" 240 160 20 Color.red;
       draw_text "Press Space to start." 190 200 20 Color.red;
       if is_key_pressed Key.Space
       then (
         game.state <- GameRunning;
         game.ball.pos <- reset_ball game.board.pos game.board.width game.board.height)
     | GamePause ->
       draw_game game;
       draw_text "Game Paused!" 240 160 20 Color.blue;
       draw_text "Press Space to continue." 190 200 20 Color.blue;
       if is_key_pressed Key.Space then game.state <- GameRunning);
    end_drawing ();
    loop game
;;

let _ =
  Printexc.record_backtrace true;
  try loop (setup 600 400) with
  | _ ->
    let backtrace = Printexc.get_raw_backtrace () in
    Printexc.print_raw_backtrace stdout backtrace;
    exit 1
;;
