(*---------------------------------*)

type t_ship_size = { nom : string; len : int }
;;

type t_params = {
  margin       : int;
  cell_size    : int;
  message_size : int;
  grid_size    : int;
  window_width : int;
  window_height: int;
  ship_sizes   : t_ship_size list;
}
;;

type t_grid = string array array;;

type t_ship = { name0 : string; pos : (int * int) list };;

type direction = Horizontal | Vertical;;

let create_grid (size : int) : t_grid =
  Array.make_matrix size size "" 
;;

(*Commentaire: La section 1 définit les types de base pour l'itération 2 du jeu "Bataille Navale".
Elle inclut :
- t_ship_size : un type représentant la taille d'un navire, avec son nom et sa longueur.
- t_params : un type regroupant les paramètres du jeu et de la fenêtre graphique, incluant la liste des tailles de navires.
- t_grid : la représentation de la grille de jeu sous forme de matrice de chaînes de caractères.
- t_ship : un type représentant un navire, défini par son nom et la liste de positions occupées.
- direction : un type énuméré indiquant l'orientation d'un navire (Horizontal ou Vertical).
La fonction create_grid initialise une grille vide de taille donnée.
@author TOTSKYI Hlib*)

let init_params () : t_params =
  let margin = 30 and cell_size = 15 and message_size = 60 and grid_size = 10 in
  let window_width = margin + cell_size * grid_size + margin + cell_size * grid_size + margin in
  let window_height = margin + message_size + cell_size * grid_size + margin in
  let ships = [ { nom="Porte-avions"; len=5 }; { nom="Croiseur"; len=4 }; { nom="Contre-torpilleur"; len=3 }; { nom="Torpilleur"; len=2 } ] in
  { margin; cell_size; message_size; grid_size; window_width; window_height; ship_sizes=ships }
;;

(*Commentaire:
- init_params calcule les dimensions de la fenêtre graphique et définit la liste des navires (Porte-avions, Croiseur, Contre-torpilleur, Torpilleur).

window_width: calcul de la largeur de la fenêtre:
- marge à gauche, grille 1, marge entre les grilles, grille 2, marge à droite

window_height: calcul de la hauteur de la fenêtre:
- marge en haut, zone de message, grille, marge en bas

@author TOTSKYI Hlib
@author AHAMADI Izaki*)

let display_empty_grids (params : t_params) : unit =
  let margin = params.margin in
  let cell_size = params.cell_size in
  let grid_size = params.grid_size in

  let x_start_left = margin in
  let y_start_left = margin in

  for i = 0 to grid_size do
    Graphics.moveto x_start_left (y_start_left + i * cell_size);
    Graphics.lineto (x_start_left + grid_size * cell_size) (y_start_left + i * cell_size)
  done;

  for j = 0 to grid_size do
    Graphics.moveto (x_start_left + j * cell_size) y_start_left;
    Graphics.lineto (x_start_left + j * cell_size) (y_start_left + grid_size * cell_size)
  done;

  let x_start_right = margin + (grid_size * cell_size) + margin in
  let y_start_right = margin in

  for i = 0 to grid_size do
    Graphics.moveto x_start_right (y_start_right + i * cell_size);
    Graphics.lineto (x_start_right + grid_size * cell_size) (y_start_right + i * cell_size)
  done;

  for j = 0 to grid_size do
    Graphics.moveto (x_start_right + j * cell_size) y_start_right;
    Graphics.lineto (x_start_right + j * cell_size) (y_start_right + grid_size * cell_size)
  done;

(*Commentaire:
display_empty_grids params dessine deux grilles vides l’une à côté de l’autre 
dans la fenêtre graphique. Chaque grille possède grid_size lignes et colonnes, 
et la taille de chaque case est déterminée par cell_size. 
Les marges (définies par margin) sont utilisées pour espacer la première grille 
du bord gauche de la fenêtre et pour séparer les deux grilles. 
On trace d’abord la grille de gauche en utilisant les coordonnées de départ 
x_start_left, y_start_left, puis on trace la grille de droite en ajoutant 
margin + grid_size * cell_size pour décaler l’origine sur l’axe des X.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)

(* --- Étiquettes --- *)
let label_left = "Ordinateur" in
let label_right = "Joueur" in

let label_y = margin + (cell_size * grid_size) + 20 in

let label_left_x =
  x_start_left + (grid_size * cell_size / 2) - (String.length label_left * 3)
in
let label_right_x =
  x_start_right + (grid_size * cell_size / 2) - (String.length label_right * 3)
in

Graphics.moveto label_left_x label_y;
Graphics.draw_string label_left;

Graphics.moveto label_right_x label_y;
Graphics.draw_string label_right;
;;

(*Commentaire:
Ce bloc de code gère l’affichage des étiquettes au-dessus des deux grilles du jeu.
Deux chaînes de caractères sont définies : "Ordinateur" pour la grille de gauche et "Joueur" pour la grille de droite.
La coordonnée verticale label_y est calculée en additionnant la marge, la hauteur de la grille (cell_size * grid_size)
et un décalage de 20 pixels, positionnant ainsi les étiquettes au-dessus des grilles.
Les coordonnées horizontales label_left_x et label_right_x sont déterminées de manière à centrer approximativement 
le texte par rapport à chaque grille. Ce calcul prend en compte l'origine de chaque grille (x_start_left et x_start_right) 
et la longueur de la chaîne (String.length multiplié par 3, pour estimer la largeur du texte).
Enfin, Graphics.moveto positionne le curseur aux coordonnées calculées et Graphics.draw_string affiche les étiquettes 
correspondantes.
@author TOTSKYI Hlib*)

(*--------------------------------------------------*)

let positions_list (x, y, len, dir : int * int * int * direction) : (int * int) list =
  let acc = ref [] in
  for i = 0 to len - 1 do
    if dir = Horizontal then acc := (x + i, y) :: !acc else acc := (x, y + i) :: !acc;
  done;
  List.rev !acc
;;

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

let random_direction () = if Random.bool () then Horizontal else Vertical
;;

let fill_positions (grid, ship_name, positions : t_grid * string * (int * int) list) : unit =
  let arr = Array.of_list positions in
  for i = 0 to Array.length arr - 1 do
    let (cx, cy) = arr.(i) in grid.(cy).(cx) <- ship_name;
  done
;;

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

(*Commentaire:
La section 2 regroupe les fonctions relatives au placement des navires sur la grille.
- positions_list calcule la liste des positions occupées par un navire à partir de sa position initiale, de sa longueur et de son orientation.
- can_place_ship vérifie que les positions d'un navire sont valides (dans la grille et sans chevauchement).
- random_direction renvoie aléatoirement une orientation (Horizontal ou Vertical).
- fill_positions inscrit le nom du navire dans les cases correspondantes de la grille.
- place_one_ship tente de placer un navire de façon aléatoire en utilisant les fonctions précédentes et réessaie en cas d'impossibilité de placement.
- auto_placing_ships place tous les navires d'une liste sur la grille de manière itérative.
@author TOTSKYI Hlib
@author AHAMADI Izaki
@author TERRENOIRE Yvan*)

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

(*Commentaire:
La section 3 gère la conversion des coordonnées de la grille en pixels et l'affichage graphique de la grille.
- cell_to_pixel convertit les coordonnées d'une cellule en coordonnées pixels en fonction des paramètres et de l'indicateur is_left (pour différencier la grille de l'ordinateur et celle du joueur).
- color_cell remplit une cellule de la grille avec une couleur donnée et dessine son contour.
- display_grid trace les lignes de la grille et, si show_ships est vrai, colore les cases contenant un navire.
@author TOTSKYI Hlib
@author AHAMADI Izaki*)

let battleship_game () : unit =
  let params = init_params () in
  open_graph (params.window_width, params.window_height); set_window_title "Bataille Navale"; Random.self_init ();;
  let grid = create_grid params.grid_size in ignore (auto_placing_ships (grid, params.ship_sizes));;
  display_grid (params, grid, true, false); display_grid (params, grid, false, true)
;;
  ignore (read_key ()); close_graph ()
;;

let () = battleship_game ()
;;

(*Commentaire:
- battleship_game ouvre la fenêtre graphique, initialise le générateur aléatoire, crée la grille, et place aléatoirement les navires.
  Ensuite, elle affiche deux grilles : celle de l'ordinateur (avec les navires masqués) et celle du joueur (avec les navires visibles).
  La fonction attend que l'utilisateur appuie sur une touche avant de fermer la fenêtre graphique.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)
