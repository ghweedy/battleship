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
};;  
(**
Commentaire :
Ce type représente les paramètres du jeu.
Il contient :
- margin : la marge autour de la zone de jeu (en pixels),
- cell_size : la taille d'une cellule de la grille (en pixels),
- message_size : la hauteur de la zone d’affichage des messages (en pixels),
- grid_size : le nombre de cellules sur un côté de la grille (int).
Le choix d'un record permet de regrouper ces valeurs de manière structurée.
*)

let init_params () : t_params =
  { margin = 30;
    cell_size = 15;
    message_size = 60;
    grid_size = 10 }
;;  
(**
Commentaire :
Cette fonction initialise et retourne une valeur de type t_params avec des valeurs par défaut.
Elle ne prend aucun paramètre, et les valeurs choisies (30, 15, 60, 10) ont été fixées pour offrir un affichage adapté.
*)

let draw_grid (x0, y0, grid_size, cell_size: int * int * int * int) : unit =
  for i = 0 to grid_size do
    let x = x0 + i * cell_size in
    moveto x y0;
    lineto x (y0 + grid_size * cell_size)
  done;
  for j = 0 to grid_size do
    let y = y0 + j * cell_size in
    moveto x0 y;
    lineto (x0 + grid_size * cell_size) y
  done
;;  
(**
Commentaire :
Cette fonction trace une grille carrée à partir du coin inférieur gauche (x0, y0).
Elle reçoit :
- x0, y0 : les coordonnées du coin inférieur gauche (int * int),
- grid_size : le nombre de cellules par ligne et colonne (int),
- cell_size : la taille de chaque cellule en pixels (int).
La fonction utilise deux boucles for :
- La première boucle trace les lignes verticales en calculant x = x0 + i * cell_size,
- La deuxième boucle trace les lignes horizontales en calculant y = y0 + j * cell_size.
Le résultat est une grille dessinée dans la fenêtre graphique.
*)

let display_grid (x, y, params, label: int * int * t_params * string) : unit =
  let nb_cells : int = params.grid_size in
  let cell_dim : int = params.cell_size in
  draw_grid x y nb_cells cell_dim;
  moveto x (y + nb_cells * cell_dim + 10);
  draw_string label  
(**
Commentaire :
Cette fonction dessine une grille et affiche une étiquette au-dessus.
Elle reçoit :
- x, y : les coordonnées du coin inférieur gauche de la grille (int * int),
- params : la structure t_params qui fournit grid_size et cell_size,
- label : la chaîne de caractères à afficher (string), par exemple "Ordinateur" ou "Joueur".
La fonction appelle d'abord draw_grid pour tracer la grille, puis positionne le curseur juste au-dessus de la grille pour afficher l'étiquette.

On utilise nb_cells et cell_dim pour rendre explicite :
nb_cells correspond au nombre de cellules dans la grille,
cell_dim correspond à la taille (en pixels) d'une cellule.
Ceci améliore la lisibilité par rapport à l'appel direct :
draw_grid x y params.grid_size params.cell_size;
*)

let display_empty_grids (params : t_params) : unit =
  clear_graph ();
  let x1 : int = params.margin in
  let y : int = params.margin + params.message_size in
  let x2 : int = params.margin * 2 + params.grid_size * params.cell_size in
  display_grid x1 y params "Ordinateur";
  display_grid x2 y params "Joueur"
;;  
(**
Commentaire :
Cette fonction efface la fenêtre graphique et affiche deux grilles vides pour le jeu.
Elle reçoit :
- params : les paramètres du jeu (t_params) utilisés pour calculer les positions des grilles.
Elle définit localement :
- x1 : la position en X de la première grille, égale à margin,
- y : la position en Y obtenue en additionnant margin et message_size (pour laisser de l’espace aux messages),
- x2 : la position en X de la deuxième grille, calculée pour être placée à droite de la première.
Ensuite, elle appelle display_grid pour afficher chaque grille avec son étiquette.
*)

let battleship_game () : unit =
  let params : t_params = init_params () in
  let win_width : int = params.margin * 3 + params.grid_size * params.cell_size * 2 in
  let win_height : int = params.margin * 2 + params.message_size + params.grid_size * params.cell_size in
  let win_dim : string = " " ^ string_of_int win_width ^ "x" ^ string_of_int win_height in
  open_graph win_dim;
  set_window_title "Bataille Navale";
  display_empty_grids params;
  let key : char = read_key () in
  close_graph ()
;;  
(**
Commentaire :
Cette fonction principale orchestre l’exécution du programme.
Elle réalise les étapes suivantes :
- Appelle init_params pour obtenir les paramètres du jeu.
- Calcule la largeur (win_width) et la hauteur (win_height) de la fenêtre en fonction de ces paramètres.
- Construit la chaîne win_dim pour spécifier les dimensions de la fenêtre graphique.
- Ouvre la fenêtre graphique avec open_graph et définit le titre via set_window_title.
- Affiche les grilles en appelant display_empty_grids.
- Attend la saisie d’une touche (stockée dans key) afin de maintenir l’affichage.
- Ferme la fenêtre graphique avec close_graph.
Les variables locales (params, win_width, win_height, win_dim, key) sont définies avec let pour structurer le code et améliorer la lisibilité.
*)

battleship_game ();;
