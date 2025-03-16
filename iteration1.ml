(*@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki*)

(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)
(* ------------------------ *)

let wait(n : int) : unit =
 Unix.sleep(n)
;;
(*permet de mettre le programme en pause pendant n secondes.*)


(* ------------------------ *)
(*        graphique         *)
(* ------------------------ *)

let open_graph (dx, dy : int * int) : unit = 
  if Sys.os_type = "Unix" then  
    let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
  else
    let s = string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
;;
(*ouvre une fenêtre graphique de dx pixels par
dy pixels. Le pixel (0, 0) est en bas à gauche, le pixel en haut à droite est à la position
(dx - 1, dy - 1)*)

let set_window_title (s : string) : unit = Graphics.set_window_title (s);;
(*modifie le titre de la fenêtre graphique à s.*)

let close_graph () : unit = Graphics.close_graph() ;;
(*ferme la fenêtre graphique*)

let clear_graph () : unit = Graphics.clear_graph() ;;
(* nettoie la fenêtre graphique.*)

let resize_window (x, y : int * int) : unit = Graphics.resize_window x y ;;
(*redimensionne la fenêtre graphique à la taille
dx par dy.*)

let moveto (x, y : int * int) : unit = Graphics.moveto x y ;;
(*positionne le point courant sur le pixel (x, y)*)

let lineto (x, y : int * int) : unit = Graphics.lineto x y ;;
(*trace une ligne de la couleur courante du point courant
jusqu’au pixel (x, y) et met à jour le point courant au pixel (x, y).*)

let plot(x, y : int * int) : unit = Graphics.plot x y ;;
(*colore le pixel (x, y) de la couleur courante*)

let current_point () : int * int = Graphics.current_point() ;;
(*retourne la position du point courant.*)

let draw_poly_line (t : (int * int) array) : unit = Graphics.draw_poly_line t ;;
(**)

let draw_circle (x, y, r : int * int * int) : unit = Graphics.draw_circle x y r ;;
(*t trace un cercle de centre (x, y) et de
rayon r. La position courante est inchangée. L’exception Invalid_argument est levée si r est
négatif.*)

let draw_ellipse (x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.draw_ellipse x y dx dy 
;;
(*trace une ellipse de centre
(x, y) et de rayons rx et ry. La position courante est inchangée. L’exception Invalid_argument
est levée si rx ou ry sont négatifs.*)

let draw_rect (x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.draw_rect x y (dx- 1) (dy - 1)
  else
    Graphics.draw_rect x (y+1) (dx-1) (dy-1)
;;
(*trace un rectangle dont le point
en bas à gauche est à la position (x, y), de largeur w et de hauteur h. La position courante
est inchangée. L’exception Invalid_argument est levée si w ou h sont négatifs.*)

let fill_rect (x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.fill_rect x y (dx- 1) (dy - 1)
  else
    Graphics.fill_rect x y dx dy
;;
(* rempli le rectangle dont le point
en bas à gauche est à la position (x, y), de largeur w et de hauteur h. La position courante
est inchangée. L’exception Invalid_argument est levée si w ou h sont négatifs.*)

let fill_poly(t : (int * int) array) : unit = Graphics.fill_poly t ;;
(**)

let fill_circle (x, y, r : int * int * int) : unit = Graphics.fill_circle x y r ;;
(*rempli le cercle de centre (x, y) et de
rayon r. La position courante est inchangée. L’exception Invalid_argument est levée si r est
négatif*)

let fill_ellipse (x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.fill_ellipse x y dx dy 
;;
(*rempli l’ellipse de centre
(x, y) et de rayons rx et ry. La position courante est inchangée. L’exception Invalid_argument
est levée si rx ou ry sont négatifs.*)

let set_line_width(e : int) : unit = Graphics.set_line_width e ;;
(**)

let draw_char (c : char) : unit = Graphics.draw_char c ;;
(*trace la lettre c à partir de la position courante en bas à gauche
de la lettre. Après exécution, la position courante est en bas à droite de la lettre.*)

let draw_string (s : string) : unit = Graphics.draw_string s ;;
(*trace le texte s à partir de la position courante en bas à
gauche du texte. Après exécution, la position courante est en bas à droite du texte.*)

let set_text_size (n : int) : unit = 
  (*unit mets à jour la taille courante d’écriture des textes.*)
  let s = "-*-courier-medium-r-*-*-"^string_of_int(n)^"-*"
  in Graphics.set_font s ;;

  type t_color = Graphics.color ;;
  (*défini les couleurs*)

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;
let grey : t_color = 128 * 256 * 256 + 128 * 256 + 128 ;;
let orange : t_color = 255 * 256 * 256 + 128 * 256 + 0 ;;
(*Les constantes black, blue, orange, red, green, white, yellow, cyan, magenta et grey
sont des couleurs prédéfinies.*)

let color_of_rgb (r, g, b : int * int * int) : t_color =
  let valid(x : int) : bool = ((0 <= x) && x <= 255) in
    if not(valid(r)) ||  not(valid(g)) || not(valid(b))
    then failwith("erreur color_of_rgb : valeurs invalides")
    else Graphics.rgb r g b
;;
(*t_color permet de définir la couleur dont la
composante rouge est r, la composante verte est g et la composante bleue est b. r, g et
b doivent être compris entre 0 et 255 et sont conformes à la représentation habituelle des couleurs RGB.*)


let set_color (color : t_color) : unit = Graphics.set_color color ;;
(*met à jour la couleur courante. La couleur de départ
est black.*)

(* ------------------------ *)
(*   controle evenements    *)
(* ------------------------ *)


let key_pressed () : bool =
  Graphics.key_pressed()
;;
(*retourne true si une touche du clavier est enfoncée et false sinon.
Elle permet d’éviter le blocage de la fonction read_key.*)

let read_key () : char =
  Graphics.read_key()
;;
(*retourne le premier caractère du tampon de saisie. Si le tampon est vide,
la fonction attend qu’une touche du clavier soit enfoncée et retourne le caractère correspondant.*)

let mouse_pos () : int * int =
  Graphics.mouse_pos()
;;
(*retourne la position courante de la souris. Si la souris est en dehors
de la fenêtre graphique, la position retournée sera aussi en dehors de la fenêtre graphique*)

let button_down () : bool = 
  Graphics.button_down()
;;
(*retourne true si un bouton de la souris est pressé et false sinon*)


(** Attend le prochain clic de sourie et retourne la position de la sourie 
    (si la sourie est hors de la fenêtre graphique, la position sera hors de l'interval 0..size_x()-1, 0..size_y()-1). *)
let wait_button_down () : int * int = 
  let statu = (Graphics.wait_next_event [Graphics.Button_down]) in
  (statu.Graphics.mouse_x, statu.Graphics.mouse_y)
;;

(*Iteration 1*)

(*1*)
type t_params = {
  margin : int;        
  cell_size : int;     
  message_size : int;  
  grid_size : int;     
  window_width : int;  
  window_height : int; 
};; 

(*Commantaire:
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
(*Commantaire:
  window_width: calcul de la largeur de la fenêtre :
    - marge à gauche, grille 1, marge entre les grilles, grille 2, marge à droite
  window_height: calcul de la hauteur de la fenêtre :
     - marge en haut, zone de message, grille, marge en bas
  @author TOTSKYI Hlib
  @author AHAMADI Izaki*)

(*3*)
let display_empty_grids (params : t_params) : unit =
  let nb_lignes : int = params.grid_size in
  let nb_colonnes : int = params.grid_size in

  
  let case_repr : string = "   " in

  let ecart : string = "      " in

  for ligne = 1 to nb_lignes do
    for colonne = 1 to nb_colonnes do
      print_string case_repr
    done;
    print_string ecart;
    for colonne = 1 to nb_colonnes do
      print_string case_repr
    done;
    print_newline ();
  done;
  
(*Commantaire:
case_repr: représentation d'une case
ecart: espace séparateur entre les deux grilles
boucle for: pour chaque ligne, on affiche: les cases de la grille de gauche,
l'espace séparateur, les cases de la grille de droite, 
puis on passe à la ligne suivante.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)

(*Affichage des étiquettes sous les grilles*)
let etiquette_gauche : string = "Ordinateur" in
let etiquette_droite : string = "Joueur" in
print_endline (etiquette_gauche ^ ecart ^ etiquette_droite)
;;

(*4*)
let battleship_game () : unit =
  let params : t_params = init_params () in
  display_empty_grids params
;; 

let () : unit =
  battleship_game ();
;;

(*Commantaire:
battleship_game: la fonction battleship_game initialise les paramètres du jeu via init_params,
puis affiche les grilles vides en appelant display_empty_grids avec ces paramètres.
fonction sans nom: point d'entrée du programme
@author TOTSKYI Hlib*)