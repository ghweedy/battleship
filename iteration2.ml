(*@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki*)

(*Iteration 2*)

(*1*)
type t_ship_size = { nom : string; len : int }

type t_params = {
  margin       : int;
  cell_size    : int;
  message_size : int;
  grid_size    : int;
  window_width : int;
  window_height: int;
  ship_sizes   : t_ship_size list;
}

type t_grid = string array array;;

type t_ship = { name0 : string; pos : (int * int) list };;

type direction = Horizontal | Vertical;;

let create_grid (size : int) : t_grid =
  Array.make_matrix size size ""
;;
(*@author Hlib TOTSKYI*)

(*2*)
(* Calcul itératif de la liste des positions d’un bateau -------- *)
let positions_list (x, y, len, dir : int * int * int * direction) : (int * int) list =
  let acc = ref [] in
  for i = 0 to len - 1 do
    if dir = Horizontal then acc := (x + i, y) :: !acc else acc := (x, y + i) :: !acc;
  done;
  List.rev !acc
;;

(* Vérifie si un bateau tient dans la grille sans chevauchement --- *)
let can_place_ship (grid, positions : t_grid * (int * int) list) : bool =
  let size = Array.length grid in
  let arr = Array.of_list positions in
  let ok = ref true in
  for i = 0 to Array.length arr - 1 do
    let (cx, cy) = arr.(i) in
    if cx < 0 || cy < 0 || cx >= size || cy >= size || grid.(cy).(cx) <> "" then ok := false;
  done;
  !ok
;;

(* Renvoie une direction aléatoire -------------------------------- *)
let random_direction () = if Random.bool () then Horizontal else Vertical
;;

(* Place les cases du bateau dans la grille ----------------------- *)
let fill_positions (grid, ship_name, positions : t_grid * string * (int * int) list) : unit =
  let arr = Array.of_list positions in
  for i = 0 to Array.length arr - 1 do
    let (cx, cy) = arr.(i) in grid.(cy).(cx) <- ship_name;
  done
;;

(* Place un bateau aléatoirement dans la grille ------------------- *)
let rec place_one_ship (grid, ship : t_grid * t_ship_size) : t_ship =
  let size = Array.length grid in
  let dir = random_direction () in
  let x = Random.int size in
  let y = Random.int size in
  let pos = positions_list (x, y, ship.len, dir) in
  if can_place_ship (grid, pos) then (
    fill_positions (grid, ship.nom, pos);
    { name0 = ship.nom; pos }
  ) else place_one_ship (grid, ship)
;;

(* Place tous les bateaux de façon itérative ---------------------- *)
let auto_placing_ships (grid, ships : t_grid * t_ship_size list) : t_ship list =
  let placed = ref [] in
  let ship_array = Array.of_list ships in
  let count = Array.length ship_array in
  let i = ref 0 in
  while !i < count do
    let ship = ship_array.(!i) in
    let p = place_one_ship (grid, ship) in
    placed := p :: !placed;
    i := !i + 1;
  done;
  List.rev !placed
;;

(*@author Hlib TOTSKYI*)

(*3*)
let cell_to_pixel (params, is_left, cx, cy : t_params * bool * int * int) : int * int =
  let origin = if is_left then params.margin else params.margin + params.grid_size * params.cell_size + params.margin in
  (origin + cx * params.cell_size, params.margin + cy * params.cell_size)
;;

let color_cell (params, is_left, cx, cy, col : t_params * bool * int * int * Graphics.color) : unit =
  let (px, py) = cell_to_pixel (params, is_left, cx, cy) in
  Graphics.set_color col; Graphics.fill_rect px py params.cell_size params.cell_size; Graphics.set_color Graphics.black; Graphics.draw_rect px py params.cell_size params.cell_size
;;

let display_grid (params, grid, is_left, show_ships : t_params * t_grid * bool * bool) : unit =
  let size = params.grid_size in
  let (x0, y0) = cell_to_pixel (params, is_left, 0, 0) in
  for i = 0 to size do Graphics.moveto x0 (y0 + i * params.cell_size); Graphics.lineto (x0 + size * params.cell_size) (y0 + i * params.cell_size) done;
  for j = 0 to size do Graphics.moveto (x0 + j * params.cell_size) y0; Graphics.lineto (x0 + j * params.cell_size) (y0 + size * params.cell_size) done;
  if show_ships then for y = 0 to size - 1 do for x = 0 to size - 1 do if grid.(y).(x) <> "" then color_cell (params, is_left, x, y, Graphics.rgb 150 200 150) done done
  ;;

  (*@author Hlib TOTSKYI*)

(*4*)
let init_params () : t_params =
  let margin = 30 and cell_size = 15 and message_size = 60 and grid_size = 10 in
  let window_width = margin + cell_size * grid_size + margin + cell_size * grid_size + margin in
  let window_height = margin + message_size + cell_size * grid_size + margin in
  let ships = [ { nom="Porte-avions"; len=5 }; { nom="Croiseur"; len=4 }; { nom="Contre-torpilleur"; len=3 }; { nom="Torpilleur"; len=2 } ] in
  { margin; cell_size; message_size; grid_size; window_width; window_height; ship_sizes=ships }
;;

let battleship_game_iteration2 () : unit =
  let params = init_params () in
  open_graph (params.window_width, params.window_height); set_window_title "Bataille Navale – Itération 2"; Random.self_init ();;
  let grid = create_grid params.grid_size in ignore (auto_placing_ships (grid, params.ship_sizes));;
  display_grid (params, grid, true, false); display_grid (params, grid, false, true)
;;
  ignore (read_key ()); close_graph ()
;;

let () = battleship_game_iteration2 ()
;;
    
(*@author Hlib TOTSKYI*)
