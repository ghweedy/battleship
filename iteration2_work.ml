(**
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki
*)

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

(**Commentaire: La section 1 définit les types de base pour l'itération 2 du jeu "Bataille Navale".
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

(**Commentaire:
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
  Graphics.draw_string label_right
;;

(**Commentaire:
display_empty_grids dessine deux grilles vides l’une à côté de l’autre 
dans la fenêtre graphique. Chaque grille possède grid_size lignes et colonnes, 
et la taille de chaque case est déterminée par cell_size. 
Les marges (définies par margin) sont utilisées pour espacer la première grille 
du bord gauche de la fenêtre et pour séparer les deux grilles. 
On trace d’abord la grille de gauche en utilisant les coordonnées de départ 
x_start_left, y_start_left, puis on trace la grille de droite en ajoutant 
margin + grid_size * cell_size pour décaler l’origine sur l’axe des X.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)

(*--------------------------------------------------*)

let positions_list (x, y, len, dir : int * int * int * direction) : (int * int) list =
  let rec pos_list_aux (i : int) : (int * int) list =
    if i = len then []
      else
        let pos : (int * int) =
          if dir = Horizontal then (x + i, y)
          else (x, y + i)
        in
      pos :: pos_list_aux (i + 1)
    in
  pos_list_aux 0
;;

let rec can_place_ship (grid, positions : t_grid * (int * int) list) : bool =
  let size : int = Array.length grid in
  if positions = [] then true
  else
    let pos : (int * int) = List.hd positions in
    let cx : int = fst pos in
    let cy : int = snd pos in
    if cx < 0 || cy < 0 || cx >= size || cy >= size || grid.(cy).(cx) <> "" then false
    else can_place_ship (grid, List.tl positions)
;;

let random_direction () : direction =
  if Random.bool () then Horizontal else Vertical
;;

let rec fill_positions (grid, ship_name, positions : t_grid * string * (int * int) list) : unit =
  if positions = [] then ()
  else
    let pos : (int * int) = List.hd positions in
    let cx : int = fst pos in
    let cy : int = snd pos in
    grid.(cy).(cx) <- ship_name;
    fill_positions (grid, ship_name, List.tl positions)
;;

let rec place_one_ship (grid, ship : t_grid * t_ship_size) : t_ship =
  let size : int = Array.length grid in
  let dir : direction = random_direction () in
  let x : int = Random.int size in
  let y : int = Random.int size in
  let pos : (int * int) list = positions_list (x, y, ship.len, dir) in
  if can_place_ship (grid, pos) then (
    fill_positions (grid, ship.nom, pos);
    { name0 = ship.nom; pos }
  ) else
    place_one_ship (grid, ship)
;;

let rec auto_placing_ships (grid, ships : t_grid * t_ship_size list) : t_ship list =
  if ships = [] then []
  else
    let ship : t_ship_size = List.hd ships in
    let placed_ship : t_ship = place_one_ship (grid, ship) in
    placed_ship :: auto_placing_ships (grid, List.tl ships)
;;

(**Commentaire:
La section 2 regroupe les fonctions relatives au placement des navires sur la grille.
- positions_list construit la liste des positions occupées par un navire à partir de sa position initiale (x, y), de sa longueur (len) et de son orientation (dir). 
  Ici, nous utilisons une fonction récursive, pos_list_aux, car nous sommes limités à l'utilisation de List.hd et List.tl pour parcourir la liste. 
  La récursion est une méthode naturelle en OCaml pour traiter des listes, car elle permet de traiter le premier élément (la tête) et de récuperer le reste de la liste (la queue) de manière simple et efficace.
- can_place_ship vérifie que les positions d'un navire sont valides (c'est-à-dire qu'elles se trouvent dans les limites de la grille et que les cases correspondantes sont libres). 
  Cette fonction parcourt la liste des positions récursivement en utilisant List.hd pour obtenir le premier élément et List.tl pour obtenir la queue, ce qui est nécessaire puisque nous ne pouvons pas utiliser d'autres fonctions de manipulation de listes.
- random_direction renvoie aléatoirement une orientation (Horizontal ou Vertical) pour le placement du navire.
- fill_positions inscrit le nom du navire (ship_name) dans les cases de la grille correspondant aux positions indiquées. 
  La récursion est utilisée ici également pour parcourir la liste des positions en traitant le premier élément puis le reste.
- place_one_ship tente de placer un navire de façon aléatoire sur la grille. 
  Elle génère aléatoirement une position de départ et une orientation, construit la liste des positions avec positions_list, et vérifie leur validité avec can_place_ship. 
  Si le placement est valide, fill_positions est appelée pour marquer la grille et la fonction renvoie un enregistrement de type t_ship contenant le nom du navire et la liste de ses positions. Sinon, la fonction se rappelle récursivement pour essayer un nouveau placement.
- auto_placing_ships parcourt la liste des navires (de type t_ship_size list) et place chacun d'eux sur la grille en appelant place_one_ship pour chaque navire. 
  La récursion est utilisée pour traiter la liste entière en se basant sur sa tête et sa queue.
@author TOTSKYI Hlib
@author AHAMADI Izaki
@author TERRENOIRE Yvan*)

let cell_to_pixel (params, is_left, cx, cy : t_params * bool * int * int) : int * int =
  let origin : int =
    if is_left then params.margin
    else params.margin + params.grid_size * params.cell_size + params.margin
  in
  (origin + cx * params.cell_size, params.margin + cy * params.cell_size)
;;

let color_cell (params, is_left, cx, cy, col : t_params * bool * int * int * Graphics.color) : unit =
  let (px, py) = cell_to_pixel (params, is_left, cx, cy) in
  Graphics.set_color col;
  Graphics.fill_rect px py params.cell_size params.cell_size;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect px py params.cell_size params.cell_size
;;

let display_grid (params, grid, is_left, show_ships : t_params * t_grid * bool * bool) : unit =
  let size : int = params.grid_size in
  let (x0, y0) = cell_to_pixel (params, is_left, 0, 0) in
  for i = 0 to size do
    Graphics.moveto x0 (y0 + i * params.cell_size);
    Graphics.lineto (x0 + size * params.cell_size) (y0 + i * params.cell_size)
  done;
  for j = 0 to size do
    Graphics.moveto (x0 + j * params.cell_size) y0;
    Graphics.lineto (x0 + j * params.cell_size) (y0 + size * params.cell_size)
  done;
  if show_ships then
    for y = 0 to size - 1 do
      for x = 0 to size - 1 do
        if grid.(y).(x) <> "" then
          color_cell (params, is_left, x, y, Graphics.rgb 150 200 150)
      done
    done
;;

(**Commentaire:
La section 3 gère la conversion des coordonnées de la grille en pixels et l'affichage graphique de la grille.
- cell_to_pixel convertit les coordonnées d'une cellule en coordonnées pixels en fonction des paramètres et de l'indicateur is_left (pour différencier la grille de l'ordinateur et celle du joueur).
- color_cell remplit une cellule de la grille avec une couleur donnée et dessine son contour.
- display_grid trace les lignes de la grille et, si show_ships est vrai, colore les cases contenant un navire.
@author TOTSKYI Hlib
@author AHAMADI Izaki*)

let battleship_game () : unit =
  let params : t_params = init_params () in
  open_graph (params.window_width, params.window_height); 
  set_window_title "Bataille Navale"; 
  Random.self_init ();
  let grid : t_grid = create_grid params.grid_size in
  ignore (auto_placing_ships (grid, params.ship_sizes));
  display_grid (params, grid, true, false); 
  display_grid (params, grid, false, true);
  ignore (read_key ());
  close_graph ()
;;
  
let () = battleship_game ()
;;

(**Commentaire:
- battleship_game ouvre la fenêtre graphique, initialise le générateur aléatoire, crée la grille, et place aléatoirement les navires.
  Ensuite, elle affiche deux grilles : celle de l'ordinateur (avec les navires masqués) et celle du joueur (avec les navires visibles).
  La fonction attend que l'utilisateur appuie sur une touche avant de fermer la fenêtre.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)
