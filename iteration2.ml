#mod_use "CPgraphics.ml";;
open CPgraphics;;
(**
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki*)

(*Iteration 2*)

type t_params = {
  margin : int;
  cell_size : int;
  message_size : int;
  grid_size : int;
  ship_sizes : (string * int) list;
};;  
(**
Commentaire :
Ce type représente les paramètres du jeu étendu.
Il contient :
- margin : la marge autour de la zone de jeu (en pixels),
- cell_size : la taille d'une cellule de la grille (en pixels),
- message_size : la hauteur de la zone d’affichage des messages (en pixels),
- grid_size : le nombre de cellules sur un côté de la grille (int),
- ship_sizes : la liste des bateaux à placer, associant chaque nom à sa longueur.
Le choix d'un record permet de regrouper ces valeurs de façon structurée.
*)

let init_params () : t_params =
  { margin = 30;
    cell_size = 15;
    message_size = 60;
    grid_size = 10;
    ship_sizes = [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Contre-torpilleur", 3); ("Torpilleur", 2)] }
;;  
(**
Commentaire :
Cette fonction initialise et retourne une valeur de type t_params avec des valeurs par défaut.
Elle ne prend aucun paramètre et fixe les dimensions ainsi que la liste des bateaux à placer.
*)

type t_grid = int array array;;
(**
Commentaire :
Ce type représente la grille de jeu sous forme d'un tableau à deux dimensions (matrice).
Chaque case est un entier : 0 signifie vide, 1 signifie occupé par un bateau.
*)

let init_grid (n : int) : t_grid =
  Array.make_matrix n n 0
;;  
(**
Commentaire :
Cette fonction crée une grille carrée de taille n×n initialisée à 0 (vide).
n est de type int.
*)

type t_ship = {
  name : string;
  positions : (int * int) list;
};;  
(**
Commentaire :
Ce type représente un bateau placé.
Il contient :
  - name : le nom du bateau (string),
  - positions : la liste des positions (int * int) que le bateau occupe.
*)

let rec positions_list (pos, dir, length,: (int * int) * int * int) : (int * int) list =
  if length = 0 then []
  else
    let (x, y) = pos in
    let next_pos =
      if dir = 0 then (x + 1, y)    
      else if dir = 1 then (x, y - 1)    
      else if dir = 2 then (x - 1, y)     
      else (x, y + 1)                    
    in
    pos :: (positions_list next_pos dir (length - 1))
;;  
(**
Commentaire :
Cette fonction récursive calcule la liste des positions qu'un bateau occupera.
Elle reçoit :
  - pos : la position de départ (int * int),
  - dir : la direction (int), codée comme suit : 0 = droite, 1 = haut, 2 = gauche, 3 = bas,
  - length : la longueur du bateau (int).
Le résultat est une liste de positions obtenue en appliquant le déplacement (next_pos) de manière répétée.
*)

let rec can_place_ship (grid, positions: t_grid * (int * int) list) : bool =
  if positions = [] then true
  else
    let (x, y) = List.hd positions in
    let n = Array.length grid in
    if x < 0 || x >= n || y < 0 || y >= n then false
    else if grid.(y).(x) <> 0 then false
    else can_place_ship grid (List.tl positions)
;;  
(**
Commentaire :
Cette fonction vérifie si un bateau peut être placé sur la grille.
Elle reçoit :
  - grid : la grille de type t_grid,
  - positions : la liste des positions que le bateau occupera.
Pour chaque position, elle teste si :
  - La position est dans les limites de la grille,
  - La case correspondante est vide (valeur 0).
Si toutes les positions sont valides, la fonction retourne true, sinon false.
*)

let rec place_ship_on_grid (grid, positions: t_grid * (int * int) list) : unit =
  if positions = [] then ()
  else
    let (x, y) = List.hd positions in
    grid.(y).(x) <- 1;
    place_ship_on_grid grid (List.tl positions)
;;  
(**
Commentaire :
Cette fonction met à jour la grille pour y placer un bateau.
Elle reçoit :
  - grid : la grille (t_grid),
  - positions : la liste des positions que le bateau occupera.
Pour chaque position, la valeur de la case est mise à 1.
La fonction est récursive et parcourt la liste des positions.
*)

let rec auto_placing_ships (grid, ships: t_grid * (string * int) list) : t_ship list =
  if ships = [] then []
  else
    let (name, len) = List.hd ships in
    let rec try_place () : (int * int) list =
      let n = Array.length grid in
      let x = Random.int n in
      let y = Random.int n in
      let dir = Random.int 4 in
      let pos_list = positions_list (x, y) dir len in
      if can_place_ship grid pos_list then pos_list
      else try_place ()
    in
    let pos = try_place () in
    place_ship_on_grid grid pos;
    { name = name; positions = pos } :: (auto_placing_ships grid (List.tl ships))
;;  
(**
Commentaire :
Cette fonction récursive place automatiquement tous les bateaux sur la grille.
Elle reçoit :
  - grid : la grille (t_grid) sur laquelle placer les bateaux,
  - ships : la liste des bateaux à placer, sous forme de couples (nom, longueur).
Pour chaque bateau, elle :
  - Tente de placer le bateau en générant aléatoirement une position (x, y) et une direction (dir) à l'aide de try_place.
  - Vérifie avec can_place_ship si le placement est possible.
  - Une fois trouvé, le bateau est placé sur la grille via place_ship_on_grid,
    et la fonction retourne un bateau de type t_ship avec son nom et la liste des positions.
La fonction utilise List.hd et List.tl pour manipuler la liste des bateaux.
*)

let cell_to_pixel (x0, y0, i, j, cell_size: int * int * int * int * int) : int * int =
  (x0 + i * cell_size, y0 + j * cell_size)
;;  
(**
Commentaire :
Cette fonction calcule la position en pixels du coin inférieur gauche d'une cellule.
Elle reçoit :
  - x0, y0 : les coordonnées du coin inférieur gauche de la grille,
  - i, j : les indices (colonne, ligne) de la cellule,
  - cell_size : la taille d'une cellule en pixels.
Le résultat est un couple (x, y) indiquant la position en pixels.
*)

let color_cell (x0, y0, i, j, cell_size, col: int * int * int * int * int * CPGraphics.color) : unit =
  set_color col;
  let (px, py) = cell_to_pixel x0 y0 i j cell_size in
  fill_rect px py cell_size cell_size
;;  
(**
Commentaire :
Cette fonction colore une case de la grille.
Elle reçoit :
  - x0, y0 : les coordonnées du coin inférieur gauche de la grille,
  - i, j : les indices de la cellule (colonne, ligne),
  - cell_size : la taille d'une cellule (int),
  - col : la couleur à appliquer (CPGraphics.color).
Elle utilise cell_to_pixel pour obtenir la position exacte puis fill_rect pour remplir la case.
*)

let display_grid_color (x0, y0, grid, cell_size: int * int * t_grid * int) : unit =
  let n = Array.length grid in
  for j = 0 to n - 1 do
    for i = 0 to n - 1 do
      if grid.(j).(i) <> 0 then
        color_cell x0 y0 i j cell_size red
      else ()
    done
  done
;;  
(**
Commentaire :
Cette fonction parcourt la grille (t_grid) et colore en rouge chaque cellule occupée (valeur différente de 0).
Elle reçoit :
  - x0, y0 : les coordonnées du coin inférieur gauche de la grille,
  - grid : la grille à afficher,
  - cell_size : la taille d'une cellule.
Les boucles imbriquées for parcourent toutes les cellules et appellent color_cell pour celles occupées.
*)

let display_empty_grids (params, comp_grid, player_grid: t_params * t_grid * t_grid) : unit =
  clear_graph ();
  let y : int = params.margin + params.message_size in
  let x_comp : int = params.margin in
  let x_player : int = params.margin * 2 + params.grid_size * params.cell_size in
  draw_grid x_comp y params.grid_size params.cell_size;
  moveto x_comp (y + params.grid_size * params.cell_size + 10);
  draw_string "Ordinateur";
  draw_grid x_player y params.grid_size params.cell_size;
  display_grid_color x_player y player_grid params.cell_size;
  moveto x_player (y + params.grid_size * params.cell_size + 10);
  draw_string "Joueur"
;;  
(**
Commentaire sur display_empty_grids (itération 2) :
Cette fonction affiche deux grilles :
  - La grille de l'ordinateur, affichée vide (les bateaux restent cachés),
  - La grille du joueur, où les bateaux sont colorés en rouge.
Les coordonnées des grilles sont calculées à partir de params :
  - x_comp : position X de la grille de l'ordinateur,
  - x_player : position X de la grille du joueur,
  - y : position Y commune (margin + message_size).
*)

let battleship_game () : unit =
  Random.self_init ();
  let params : t_params = init_params () in
  let grid_size = params.grid_size in
  let comp_grid : t_grid = init_grid grid_size in
  let player_grid : t_grid = init_grid grid_size in
  (* Placement automatique des bateaux pour l'ordinateur et pour le joueur.
     On utilise la même méthode pour placer les bateaux sur chaque grille. *)
  let _comp_ships = auto_placing_ships comp_grid params.ship_sizes in
  let _player_ships = auto_placing_ships player_grid params.ship_sizes in
  let win_width : int = params.margin * 3 + params.grid_size * params.cell_size * 2 in
  let win_height : int = params.margin * 2 + params.message_size + params.grid_size * params.cell_size in
  let win_dim : string = " " ^ string_of_int win_width ^ "x" ^ string_of_int win_height in
  open_graph win_dim;
  set_window_title "Battleship - Itération 2";
  display_empty_grids params comp_grid player_grid;
  let _key : char = read_key () in  (* Attente d'une touche pour maintenir l'affichage *)
  close_graph ()
;;  
(**
Commentaire :
Elle réalise les étapes suivantes :
  - Initialise les paramètres avec init_params.
  - Crée deux grilles (une pour l'ordinateur et une pour le joueur) en appelant init_grid.
  - Place automatiquement les bateaux sur chaque grille avec auto_placing_ships.
  - Calcule les dimensions de la fenêtre en fonction des paramètres.
  - Ouvre la fenêtre graphique et définit le titre.
  - Affiche les deux grilles à l'aide de display_empty_grids : la grille de l'ordinateur reste vide, 
    celle du joueur affiche les bateaux en coloriant les cases en rouge.
  - Attend la saisie d'une touche par l'utilisateur, puis ferme la fenêtre.
Les variables locales sont définies avec let pour structurer le code et améliorer sa lisibilité.
*)

battleship_game ();;
