(**
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki
*)


(*Iteration 1*)

(*1*)
type t_params = {
  margin : int;        
  cell_size : int;     
  message_size : int;  
  grid_size : int;     
  window_width : int;  
  window_height : int; 
  ship_size: int;
}
;; 

(**Commentaire:
margin: largeur des marges, 30 pixels
cell_size: largeur des cases, 15 pixels
message_size: hauteur de la zone de message, 60 pixels
grid_size: taille des grilles en nombre de cases, 10 pixels
window_width: largeur totale de la fenêtre graphique
window_height: hauteur totale de la fenêtre graphique
@author TOTSKYI Hlib*)

(*2*)
let init_params () : t_params =
  let margin : int = 30 in
  let cell_size : int = 15 in
  let message_size : int = 60 in
  let grid_size : int = 10 in

  let window_width =
    margin + (cell_size * grid_size) + margin + (cell_size * grid_size) + margin
  in
  let window_height =
    margin + message_size + (cell_size * grid_size) + margin
  in
  { margin; cell_size; message_size; grid_size; window_width; window_height }
;;

(**Commentaire:
  window_width: calcul de la largeur de la fenêtre :
    - marge à gauche, grille 1, marge entre les grilles, grille 2, marge à droite
  window_height: calcul de la hauteur de la fenêtre :
     - marge en haut, zone de message, grille, marge en bas
  @author TOTSKYI Hlib
  @author AHAMADI Izaki*)

(*3*)
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

(**Commentaire:
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

(* Dessin des étiquettes *)
Graphics.moveto label_left_x label_y;
Graphics.draw_string label_left;

Graphics.moveto label_right_x label_y;
Graphics.draw_string label_right;
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

(*4*)
let battleship_game () =
  let params = init_params () in
  let open_graph_command = Printf.sprintf " %dx%d" params.window_width params.window_height in
  Graphics.open_graph open_graph_command;
  Graphics.set_window_title "Bataille Navale";
  display_empty_grids params;
  ignore (Graphics.read_key ());
  Graphics.close_graph ();
;;

let () = battleship_game (); 

(**Commentaire:
battleship_game est la fonction principale du jeu "Bataille Navale". 
Elle effectue les opérations suivantes :
Initialise les paramètres du jeu en appelant init_params, qui détermine la taille 
des grilles, la marge, la taille des cellules, etc.
Construit la commande d'ouverture de la fenêtre graphique en fonction des dimensions 
calculées window_width et window_height.
Ouvre la fenêtre graphique avec Graphics.open_graph et définit le titre de la fenêtre 
grâce à Graphics.set_window_title.
Affiche les deux grilles vides côte à côte en appelant display_empty_grids params.
Attend que l'utilisateur appuie sur une touche avec Graphics.read_key afin 
de permettre la visualisation avant la fermeture.
Ferme la fenêtre graphique avec Graphics.close_graph.
@author TOTSKYI Hlib*)



