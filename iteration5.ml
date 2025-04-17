#mod_use "CPgraphics.ml";;
open CPgraphics;;

(**
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki
*)

type t_params = {
  margin : int;
  cell_size : int;
  message_size : int;
  grid_size : int;
  ship_sizes : (string * int) list;
}
;;  
(**
Commentaire :
Ce type représente les paramètres du jeu étendu.
Il contient :
  - margin : la marge autour de la zone de jeu (30 pixels),
  - cell_size : la taille d'une cellule de la grille (15 pixels),
  - message_size : la hauteur de la zone d’affichage des messages (60 pixels),
  - grid_size : le nombre de cellules sur un côté de la grille (10, soit une grille 10×10),
  - ship_sizes : la liste des bateaux à placer, associant chaque nom à sa longueur.
Le choix d'un record permet de regrouper ces valeurs de façon structurée.
@author TOTSKYI Hlib
*)

let init_params () : t_params =
  { margin = 30;
    cell_size = 15;
    message_size = 60;
    grid_size = 10;
    ship_sizes = [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Contre-torpilleur", 3); ("Torpilleur", 2)]}
;;

type t_grid = int array array;;


let init_grid (n : int) : t_grid =
  Array.make_matrix n n 0
;;

type t_ship = {
  name : string;
  positions : (int * int) list;
}
;;
(**
Commentaire :
La fonction init_params initialise et retourne une structure de type t_params, qui regroupe 
tous les paramètres essentiels du jeu. Elle définit :
- margin : la marge autour de la zone de jeu (30 pixels),
- cell_size : la taille d'une cellule de la grille (15 pixels),
- message_size : la hauteur de la zone d’affichage des messages (60 pixels),
- grid_size : le nombre de cellules sur un côté de la grille (10, donc une grille 10x10),
- ship_sizes : la liste des bateaux à placer, associant à chaque nom sa longueur.   
Le type t_grid représente la grille de jeu sous forme d'une matrice d'entiers. 
Chaque cellule de cette matrice est un entier dont la valeur 0 signifie que la case est vide, 
tandis qu'une valeur différente de 0 indique qu'elle est occupée par un bateau.
La fonction init_grid reçoit un entier n et crée une grille carrée de taille n×n, initialisée avec 0 
dans toutes ses cases.
Le type t_ship permet de représenter un bateau placé sur la grille. Il s'agit d'un record qui contient :
- name : le nom du bateau,
- positions : une liste de tuples (int * int) indiquant les positions exactes que le bateau occupe 
sur la grille.
@author TOTSKYI Hlib
@author AHAMADI Izaki
*)

let draw_grid (x0, y0, grid_size, cell_size : int * int * int * int) : unit =
  for i = 0 to grid_size do
    let x = x0 + i * cell_size in
    moveto (x, y0);
    lineto (x, y0 + grid_size * cell_size)
  done;
  for j = 0 to grid_size do
    let y = y0 + j * cell_size in
    moveto (x0, y);
    lineto (x0 + grid_size * cell_size, y) 
  done
;;

let display_grid (x, y, params, label : int * int * t_params * string) : unit =
  draw_grid (x, y, params.grid_size, params.cell_size);
  moveto (x, y + params.grid_size * params.cell_size + 10);
  draw_string label
;;

let rec positions_list (pos, dir, length : (int * int) * int * int) : (int * int) list =
  if length = 0 then []
  else
    let (x, y) = pos in
    let next_pos =
      if dir = 0 then (x + 1, y)
      else if dir = 1 then (x, y - 1)
      else if dir = 2 then (x - 1, y)
      else (x, y + 1)
    in
    pos :: (positions_list (next_pos, dir, length - 1))
;;
(**
Commentaire :
Cette fonction récursive calcule la liste des positions qu'un bateau occupera.
Elle reçoit :
- pos : la position de départ (int * int),
- dir : la direction (int), codée comme suit : 0 = droite, 1 = haut, 2 = gauche, 3 = bas,
- length : la longueur du bateau (int).
Le résultat est une liste de positions obtenue en appliquant le déplacement (next_pos) de manière répétée.
@author TOTSKYI Hlib
@author AHAMADI Izaki
*)

let rec can_place_ship (grid, positions : t_grid * (int * int) list) : bool =
  if positions = [] then true
  else
    let (x, y) = List.hd positions in
    let n = Array.length grid in
    if x < 0 || x >= n || y < 0 || y >= n then false
    else if grid.(y).(x) <> 0 then false
    else can_place_ship (grid, List.tl positions)
;;
(**
Commentaire :
Cette fonction vérifie si un bateau peut être placé sur la grille.
Elle reçoit :
- grid : la grille (t_grid),
- positions : la liste des positions que le bateau occupera.
Pour chaque position, elle teste si :
- La position est dans les limites de la grille,
- La case correspondante est vide (valeur 0).
Si toutes les positions sont valides, la fonction retourne true, sinon false.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
*)

let rec place_ship_on_grid (grid, positions : t_grid * (int * int) list) : unit =
  if positions = [] then ()
  else
    let (x, y) = List.hd positions in
    grid.(y).(x) <- 1;
    place_ship_on_grid (grid, List.tl positions)
;;

(**
Commentaire :
Cette fonction met à jour la grille pour y placer un bateau.
Elle reçoit :
- grid : la grille (t_grid),
- positions : la liste des positions que le bateau occupera.
Pour chaque position, la valeur de la case est mise à 1.
La fonction est récursive et parcourt la liste des positions.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
*)

let rec auto_placing_ships (grid, ships : t_grid * (string * int) list) : t_ship list =
  if ships = [] then []
  else
    let (name, len) = List.hd ships in
    let rec try_place () : (int * int) list =
      let n = Array.length grid in
      let x = Random.int n in
      let y = Random.int n in
      let dir = Random.int 4 in
      let pos_list = positions_list ((x, y), dir, len) in
      if can_place_ship (grid, pos_list) then pos_list
      else try_place ()
    in
    let pos = try_place () in
    place_ship_on_grid (grid, pos);
    { name = name; positions = pos } :: (auto_placing_ships (grid, List.tl ships))
;;
(**
Commentaire :
Cette fonction récursive place automatiquement tous les bateaux sur la grille.
Elle reçoit :
- grid : la grille (t_grid) sur laquelle placer les bateaux,
- ships : la liste des bateaux à placer, sous forme de couples (nom, longueur).
Pour chaque bateau, elle :
- Tente de placer le bateau en générant aléatoirement une position (x, y) et une direction (dir) avec try_place.
- Vérifie avec can_place_ship si le placement est possible.
- Une fois trouvé, le bateau est placé sur la grille via place_ship_on_grid,
et la fonction retourne un bateau de type t_ship avec son nom et sa liste de positions.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
*)

let cell_to_pixel (x0, y0, i, j, cell_size : int * int * int * int * int) : int * int =
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
@author TOTSKYI Hlib
@author AHAMADI Izaki
*)

let color_cell (x0, y0, i, j, cell_size, col : int * int * int * int * int * Graphics.color) : unit =
  set_color col;
  let (px, py) = cell_to_pixel (x0, y0, i, j, cell_size) in
  fill_rect (px, py, cell_size, cell_size)
;;
(**
Commentaire :
Cette fonction colore une case de la grille.
Elle reçoit :
  - x0, y0 : les coordonnées du coin inférieur gauche de la grille,
  - i, j : les indices de la cellule (colonne, ligne),
  - cell_size : la taille d'une cellule (int),
  - col : la couleur à appliquer (Graphics.color).
Elle utilise cell_to_pixel pour obtenir la position exacte, puis fill_rect pour remplir la case.
@author AHAMADI Izaki
@author TERRENOIRE Yvan
*)

let display_grid_color (x0, y0, grid, cell_size : int * int * t_grid * int) : unit =
  let n = Array.length grid in
  for j = 0 to n - 1 do
    for i = 0 to n - 1 do
      let cell_val = grid.(j).(i) in
      if cell_val = 2 then
         color_cell (x0, y0, i, j, cell_size, green)
      else if cell_val = 3 then
         color_cell (x0, y0, i, j, cell_size, orange)
      else if cell_val = 4 then
         color_cell (x0, y0, i, j, cell_size, red)
      else ()
    done
  done
;;
(**
Commentaire :
Cette version de display_grid_color parcourt la grille et colore les cases en fonction de leur état :
- 0 pour l'eau (pas de couleur),
- 1 pour bateau non touché (pas de couleur),
- 2 pour tir manqué (vert),
- 3 pour bateau touché (orange),
- 4 pour bateau coulé (rouge).
@author TOTSKYI Hlib
*)

let display_message (msgs, params, win_width : string list * t_params * int) : unit =
  set_color Graphics.white;
  fill_rect  (params.margin, 0, win_width - 2 * params.margin, params.message_size);
  set_color Graphics.black;
  let rec aux (l, y: string list * int) : unit =
    if l = [] then () 
    else (
      moveto (params.margin + 5, y);
      draw_string (List.hd l);
      aux (List.tl l, y - 15)
    )
  in
  aux (msgs, params.margin + params.message_size - 15)
;;
(**
Commentaire :
Cette fonction affiche une liste de messages dans la zone réservée aux messages.
Elle reçoit :
- msgs : la liste des chaînes de caractères à afficher,
- params : la structure t_params (pour obtenir margin et message_size),
- win_width : la largeur totale de la fenêtre.
Elle efface d'abord la zone de messages en la remplissant en blanc, puis affiche chaque ligne de message en noir.
La fonction auxiliaire aux (récursive) permet d'afficher chaque message sur une ligne distincte.
@author TOTSKYI Hlib
*)

let read_mouse (params : t_params) : (int * int) =
  let event = Graphics.wait_next_event [Button_down] in
  let mx = event.mouse_x and my = event.mouse_y in
  let x_player = params.margin * 2 + params.grid_size * params.cell_size in
  let y_grid = params.margin + params.message_size in
  if mx >= x_player && mx < x_player + params.grid_size * params.cell_size &&
     my >= y_grid && my < y_grid + params.grid_size * params.cell_size then
       let i = (mx - x_player) / params.cell_size in
       let j = (my - y_grid) / params.cell_size in
       (i, j)
  else
       (-1, -1)
;;
(**
Commentaire :
Cette fonction attend le prochain clic de souris et retourne la position de la case cliquée dans la grille du joueur.
Elle reçoit :
- params : la structure t_params pour calculer la zone de la grille du joueur.
La zone de la grille du joueur est déterminée par :
- x_player = margin * 2 + grid_size * cell_size,
- y_grid = margin + message_size.
Si le clic se produit à l'intérieur de la grille, la fonction calcule et renvoie les indices (i, j) de la cellule cliquée.
Sinon, elle retourne (-1, -1) pour indiquer un clic en dehors de la grille.
@author TOTSKYI Hlib
*)

let rec manual_placing_ship_list (grid, ships, params : t_grid * (string * int) list * t_params) : t_ship list =
  if ships = [] then []
  else
    let (name, len) = List.hd ships in
    let win_width = params.margin * 3 + params.grid_size * params.cell_size * 2 in
    display_message (["Placez le " ^ name ^ " (longueur " ^ string_of_int len ^ ")"], params, win_width);
    let first_cell = read_mouse params in
    if first_cell = (-1, -1) then
      manual_placing_ship_list (grid, ships, params)
    else
      let second_cell = read_mouse params in
      if second_cell = (-1, -1) then
        manual_placing_ship_list (grid, ships, params)
      else
        let (i1, j1) = first_cell in
        let (i2, j2) = second_cell in
        let dir =
          if i2 = i1 + 1 && j2 = j1 then 0
          else if i2 = i1 && j2 = j1 - 1 then 1
          else if i2 = i1 - 1 && j2 = j1 then 2
          else if i2 = i1 && j2 = j1 + 1 then 3
          else -1
        in
        if dir = -1 then (
          display_message (["Erreur: case non contigue. Réessayez."], params, win_width);
          manual_placing_ship_list (grid, ships, params)
        )
        else
          let pos_list = positions_list ((i1, j1), dir, len) in
          if not (can_place_ship (grid, pos_list)) then (
            display_message (["Placement impossible. Réessayez."], params, win_width);
            manual_placing_ship_list (grid, ships, params)
          )
          else (
            place_ship_on_grid (grid, pos_list);
            let rec color_positions (lst : (int * int) list) : unit =
              if lst = [] then () else (
                let (i, j) = List.hd lst in
                let x_player = params.margin * 2 + params.grid_size * params.cell_size in
                let y_grid = params.margin + params.message_size in
                color_cell (x_player, y_grid, i, j, params.cell_size, yellow);
                color_positions (List.tl lst)
              )
            in
            color_positions pos_list;
            { name = name; positions = pos_list } :: (manual_placing_ship_list (grid, List.tl ships, params))
          )
;;
(**
Commentaire :
Cette fonction récursive permet au joueur de placer manuellement ses bateaux sur la grille.
Elle reçoit :
- grid : la grille du joueur (t_grid),
- ships : la liste des bateaux à placer, sous forme de couples (nom, longueur),
- params : les paramètres du jeu (t_params).
Pour chaque bateau, la fonction :
- Affiche un message indiquant quel bateau placer et sa longueur (à l'aide de display_message),
- Attend que l'utilisateur clique sur une cellule pour définir la position de départ,
- Attend un second clic pour déterminer la direction. La direction est calculée en comparant les indices des deux cellules.
- Si la deuxième cellule n'est pas contiguë à la première, un message d'erreur est affiché et le placement recommence.
- Si le placement est possible (vérifié par can_place_ship), le bateau est placé sur la grille (place_ship_on_grid),
et les cellules correspondantes sont coloriées en jaune.
Le bateau ainsi placé est ajouté à la liste des bateaux placés, et la fonction se rappelle récursivement pour le bateau suivant.
@author TOTSKYI Hlib
@author AHAMADI Izaki
@author TERRENOIRE Yvan
*)

let rec exists (x, lst: 'a * 'a list) : bool =
  if lst = [] then false
  else if List.hd lst = x 
    then true
     else 
    exists (x, List.tl lst)
;;
(**
Commentaire :
exists vérifie récursivement si un élément x existe dans la liste lst.
Le paramètre x est polymorphe ('a) pour être compatible avec tout type.
@author TOTSKYI Hlib
*)

let rec all_listes (f, lst: (int * int) * (int * int) list) : bool =
  if lst = [] 
  then true
  else if f = (List.hd lst) 
  then all_listes (f, List.tl lst)
  else
   false
;;
(**
Commentaire :
all_listes vérifie si la fonction f est vraie pour tous les éléments de la liste lst.
@author TOTSKYI Hlib
*)

let rec appl (f, lst: 'a * 'a list) : unit =
  if lst = [] then ()
   else 
    ( (f, List.hd lst); appl (f, List.tl lst))
;;
(**
Commentaire :
appl applique une fonction f à chaque élément de la liste lst, en utilisant la récursion.
@author TOTSKYI Hlib
*)

let check_cell (grid, pos: t_grid * (int * int)) : bool =
  let (i, j) : int * int = pos in
  grid.(j).(i) = 3
;;

(**
Commentaire :
check_cell retourne true si la case (i, j) dans grid a la valeur 3 (bateau touché).
@author AHAMADI Izaki
*)

let check_sunk_ship (ship, grid : t_ship * t_grid) : bool =
  let p_f (pos: (int * int)) = 
    check_cell (grid, pos) in
  all_listes (p_f, ship.positions)
;;
(**
check_sunk_ship retourne true si toutes les positions du bateau sont à 3 dans grid, indiquant qu'il est coulé.
@author TERRENOIRE Yvan
*)

let set_cell (grid, pos : t_grid * (int * int)) : unit =
  let (i, j) : int * int = pos in
  grid.(j).(i) <- 4
;;
(**
Commentaire :
set_cell modifie la case correspondant à pos dans grid en lui assignant 4, indiquant que le bateau est coulé.
@author AHAMADI Izaki
*)

let sink_ship (ship, grid : t_ship * t_grid) : unit =
  let p_f0 (pos: (int * int)) =
    set_cell (grid, pos) in
  appl (p_f0, ship.positions)
;;
(**
Commentaire :
sink_ship parcourt la liste des positions du bateau et met à jour chacune avec set_cell.
@author TERRENOIRE Yvan
*)

let update_grid (grid, (i, j): t_grid * (int * int)) : unit =
  let cell_val : int = grid.(j).(i) in
  if cell_val = 0 then
    grid.(j).(i) <- 2   
  else if cell_val = 1 then
    grid.(j).(i) <- 3   
  else
    ()
;;
(**
Commentaire :
update_grid modifie la valeur d'une cellule en fonction du tir :
- Si la cellule est vide (0), elle devient 2 (tir manqué).
- Si elle contient un bateau (1), elle devient 3 (bateau touché).
@author TERRENOIRE Yvan
@author AHAMADI Izaki
*)

let rec find_ship (ships, pos : t_ship list * (int * int)) : t_ship =
  if ships = [] 
    then { name = "ship"; positions = [] }
  else
    let ship : t_ship = List.hd ships in
    if exists (pos, ship.positions) 
      then ship
    else find_ship (List.tl ships, pos) 
;;
(**
Commentaire :
find_ship recherche dans la liste des bateaux le bateau dont les positions contiennent pos.
Si aucun bateau n'est trouvé, un bateau "ship" est retourné.
@author TOTSKYI Hlib
*)

let player_shoot (grid, ships, params: t_grid * t_ship list * t_params) : t_ship list =
  let (i, j) : int * int = read_mouse.params in
  if i = -1 && j = -1 then
    ships
  else
    let cell_val : int = grid.(j).(i) in
    if cell_val = 2 || cell_val = 3 || cell_val = 4 then
      ships
    else
      let update_result : unit = update_grid (grid, (i, j)) in
      if grid.(j).(i) = 3 then
        let found_ship : t_ship = find_ship (ships, (i, j)) in
        if check_sunk_ship (found_ship, grid) then
          let sink_result : unit = sink_ship (found_ship, grid) in
          let message_result : unit = display_message (["Bateau coulé !"], params, (params.margin * 3 + params.grid_size * params.cell_size * 2)) in
          ships
        else
          ships
      else
        ships
;;
(**
Commentaire :
player_shoot permet au joueur de tirer sur la grille.  
Elle lit la position du clic via read_mouse et, selon la valeur de la cellule ciblée :
- Met à jour la cellule (0 → 2 ou 1 → 3) avec update_grid,
- Si le tir touche un bateau (la cellule passe à 3), elle recherche le bateau via find_ship, 
vérifie s'il est coulé avec check_sunk_ship et, le cas échéant, applique sink_ship.
La fonction retourne la liste des bateaux sans modification.
@author TOTSKYI Hlib
*)

let rec player_turns (grid, ships, params, n: t_grid * t_ship list * t_params * int) : unit =
  if n = 0 then
    ()
  else
    let updated_ships : t_ship list = 
      player_shoot (grid, ships, params) in
    let x_player : int = params.margin * 2 + params.grid_size * params.cell_size in
    let y_grid : int = params.margin + params.message_size in
    let l_u0 : unit = draw_grid (x_player, y_grid, params.grid_size, params.cell_size) in
    let l_u1 : unit = display_grid_color (x_player, y_grid, grid, params.cell_size) in
    player_turns (grid, updated_ships, params, (n - 1))
;;
(**
Commentaire :
player_turns permet d'effectuer n tirs successifs du joueur.
Après chaque tir, la grille est redessinée pour refléter les changements (tir manqué, touché ou coulé).
@author TOTSKYI Hlib
*)

let check_cell_x (grid, pos : t_grid * (int * int)) : bool =
  let (i, j) : int * int = pos in
  grid.(j).(i) = 4
;;
(**
Commentaire :
check_cell_x retourne true si la cellule (i, j) vaut 4, c'est-à-dire si le bateau est coulé.
@author TERRENOIRE Yvan
*)

let ship_sunk (ship, grid : t_ship * t_grid) : bool =
  let p_f1 (pos : int * int) = 
    check_cell_x (grid, pos) in
  all_listes (p_f1, ship.positions)
;;
(**
Commentaire :
ship_sunk vérifie que toutes les cellules occupées par le bateau sont à 4, indiquant qu'il est coulé.
@author AHAMADI Izaki
*)

let rec all_sunk (ships, grid : t_ship list * t_grid) : bool =
  if ships = [] then true
  else if ship_sunk (List.hd ships, grid) then
    all_sunk (List.tl ships, grid)
  else false
;;
(**
Commentaire :
all_sunk retourne true si tous les bateaux de la liste sont coulés (toutes leurs cellules valent 4).
@author AHAMADI Izaki
@author TERRENOIRE Yvan
*)

let rec computer_shoot (grid, ships, params : t_grid * t_ship list * t_params) : t_ship list =
  let n : int = Array.length grid in
  let i : int = Random.int n in
  let j : int = Random.int n in
  if grid.(j).(i) = 2 || grid.(j).(i) = 3 || grid.(j).(i) = 4 then
    computer_shoot (grid, ships, params)
  else
    let () : unit = update_grid (grid, (i, j)) in
    if grid.(j).(i) = 3 then
      let found_ship : t_ship = find_ship (ships, (i, j)) in
      if check_sunk_ship (found_ship, grid) then
        let () : unit = sink_ship (found_ship, grid) in
        ships
      else
        ships
    else
      ships
;;
(**
Commentaire :
computer_shoot permet à l'ordinateur de tirer aléatoirement sur la grille.
Si la cellule choisie a déjà été tirée, il recommence.
Si le tir touche un bateau (cellule passe à 3), il cherche le bateau via find_ship, vérifie s'il est coulé,
et le marque comme coulé avec sink_ship si nécessaire.
@author TOTSKYI Hlib
*)

let rec all_shoot (game, params : t_battleship * t_params) : unit =
  if all_sunk (game.comp_ships, game.comp_grid) then
    display_message (["Vous avez gagné !"], params, params.margin * 3 + params.grid_size * params.cell_size * 2)
  else if all_sunk (game.player_ships, game.player_grid) then
    display_message (["Vous avez perdu !"], params, params.margin * 3 + params.grid_size * params.cell_size * 2)
  else
    let updated_comp_ships : t_ship list = player_shoot (game.comp_grid, game.comp_ships, params) in
    let updated_player_ships : t_ship list = computer_shoot (game.player_grid, game.player_ships, params) in
    let x_comp : int = params.margin in
    let x_player : int = params.margin * 2 + params.grid_size * params.cell_size in
    let y_grid : int = params.margin + params.message_size in
    draw_grid (x_comp, y_grid, params.grid_size, params.cell_size);
    display_grid_color (x_comp, y_grid, game.comp_grid, params.cell_size);
    moveto (x_comp, y_grid + params.grid_size * params.cell_size + 10);
    draw_string "Ordinateur";
    draw_grid (x_player, y_grid, params.grid_size, params.cell_size);
    display_grid_color (x_player, y_grid, game.player_grid, params.cell_size);
    moveto (x_player, y_grid + params.grid_size * params.cell_size + 10);
    draw_string "Joueur";
    all_shoot ({
      comp_grid = game.comp_grid;
      player_grid = game.player_grid;
      comp_ships = updated_comp_ships;
      player_ships = updated_player_ships
    }, params)
;;
(**
Commentaire :
all_shoot alterne les tirs du joueur et de l'ordinateur jusqu'à ce que tous les bateaux de l'une des parties soient coulés.
Si tous les bateaux de l'ordinateur sont coulés, le message "Vous avez gagné !" s'affiche.
Si tous les bateaux du joueur sont coulés, le message "Vous avez perdu !" s'affiche.
Sinon, on effectue un tour de tirs pour chaque côté et on recommence récursivement.
@author TOTSKYI Hlib
*)

type t_battleship = {
  comp_grid : t_grid;
  player_grid : t_grid;
  comp_ships : t_ship list;
  player_ships : t_ship list;
};;  
(**
Commentaire :
Ce type structuré regroupe les éléments essentiels du jeu :
- comp_grid : la grille de l'ordinateur (t_grid),
- player_grid : la grille du joueur (t_grid),
- comp_ships : la liste des bateaux placés pour l'ordinateur (t_ship list),
- player_ships : la liste des bateaux placés pour le joueur (t_ship list).
Il sert à stocker l'état initial du jeu après le placement des bateaux.
@author TOTSKYI Hlib
*)

let init_battleship (params : t_params) : t_battleship =
  let grid_size : int = params.grid_size in
  let comp_grid : t_grid = init_grid grid_size in
  let player_grid : t_grid = init_grid grid_size in
  let comp_ships : t_ship list = auto_placing_ships (comp_grid, params.ship_sizes) in
  let player_ships : t_ship list = auto_placing_ships (player_grid, params.ship_sizes) in
  {
    comp_grid = comp_grid;
    player_grid = player_grid;
    comp_ships = comp_ships;
    player_ships = player_ships
  };;
(**
Commentaire :
Cette fonction initialise l'état du jeu (t_battleship).
Elle réalise les étapes suivantes :
- Crée deux grilles (une pour l'ordinateur et une pour le joueur) à l'aide de init_grid.
- Place automatiquement les bateaux sur la grille de l'ordinateur avec auto_placing_ships.
- Permet au joueur de placer manuellement ses bateaux sur sa grille via manual_placing_ship_list.
Le résultat est un enregistrement de type t_battleship contenant les grilles et les listes de bateaux placés.
@author TOTSKYI Hlib
*)

let battleship_game () : unit =
  Random.self_init ();
  let params : t_params = init_params () in
  let comp_grid : t_grid = init_grid params.grid_size in
  let player_grid : t_grid = init_grid params.grid_size in
  let comp_ships : t_ship list = auto_placing_ships (comp_grid, params.ship_sizes) in
  let player_ships : t_ship list = auto_placing_ships (player_grid, params.ship_sizes) in
  let game : t_battleship = {
    comp_grid = comp_grid;
    player_grid = player_grid;
    comp_ships = comp_ships;
    player_ships = player_ships
  } in
  let win_width : int = params.margin * 3 + params.grid_size * params.cell_size * 2 in
  let win_height : int = params.margin * 2 + params.message_size + params.grid_size * params.cell_size in
  let win_dim : string = " " ^ string_of_int win_width ^ "x" ^ string_of_int win_height in
  open_graph win_dim;
  set_window_title "Bataille Navale";
  let y_grid : int = params.margin + params.message_size in
  let x_comp : int = params.margin in
  let x_player : int = params.margin * 2 + params.grid_size * params.cell_size in
  draw_grid (x_comp, y_grid, params.grid_size, params.cell_size);
  display_grid_color (x_comp, y_grid, game.comp_grid, params.cell_size);
  moveto (x_comp, y_grid + params.grid_size * params.cell_size + 10);
  draw_string "Ordinateur";
  draw_grid (x_player, y_grid, params.grid_size, params.cell_size);
  display_grid_color (x_player, y_grid, game.player_grid, params.cell_size);
  moveto (x_player, y_grid + params.grid_size * params.cell_size + 10);
  draw_string "Joueur";
  all_shoot (game, params);
  let key : int = read_key () in
  close_graph ();;
(**
Commentaire :
battleship_game orchestre le jeu complet en alternant les tirs du joueur et de l'ordinateur jusqu'à ce qu'une partie gagne.
Elle :
  - Initialise les paramètres et l'état du jeu via init_params et init_battleship.
  - Calcule et ouvre une fenêtre graphique avec les dimensions appropriées (par exemple 390×270 pixels).
  - Affiche les deux grilles (ordinateur et joueur) avec leurs bateaux (les bateaux de l'ordinateur restent cachés dans le jeu complet, mais ici ils sont affichés pour le test).
  - Appelle all_shoot pour alterner les tirs jusqu'à ce qu'une des parties ait tous ses bateaux coulés.
  - Attend une saisie clavier avant de fermer la fenêtre.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki
*)

battleship_game ();;
