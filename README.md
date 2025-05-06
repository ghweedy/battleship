Documentation extraite automatiquement du fichier battleship.ml
============================================================

Bloc 1:
@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki

Bloc 2:
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

Bloc 3:
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

Bloc 4:
Commentaire :
Cette fonction trace une grille carrée composée de lignes horizontales et verticales.
Elle reçoit :
- x0, y0 : les coordonnées du coin inférieur gauche de la grille (int * int),
- grid_size : le nombre de cellules par côté (int),
- cell_size : la taille d’une cellule en pixels (int).
Elle utilise deux boucles :
- La première pour dessiner les lignes verticales,
- La seconde pour dessiner les lignes horizontales.
Le résultat est une grille graphique de dimensions grid_size × grid_size.
@author TOTSKYI Hlib

Bloc 5:
Commentaire :
Cette fonction affiche une grille vide accompagnée d’un label au-dessus.
Elle reçoit :
- x, y : les coordonnées du coin inférieur gauche de la grille (int * int),
- params : une structure contenant les paramètres du jeu, notamment la taille des cellules et de la grille (t_params),
- label : le texte à afficher au-dessus de la grille (string).
La fonction appelle :
- draw_grid pour dessiner la grille à la position (x, y),
- moveto puis draw_string pour afficher le label centré au-dessus de la grille.
Cette procédure est utilisée pour afficher soit la grille du joueur, soit celle de l’ordinateur.
@author TOTSKYI Hlib

Bloc 6:
Commentaire :
Cette fonction récursive calcule la liste des positions qu'un bateau occupera.
Elle reçoit :
- pos : la position de départ (int * int),
- dir : la direction (int), codée comme suit : 0 = droite, 1 = haut, 2 = gauche, 3 = bas,
- length : la longueur du bateau (int).
Le résultat est une liste de positions obtenue en appliquant le déplacement (next_pos) de manière répétée.
@author TOTSKYI Hlib
@author AHAMADI Izaki

Bloc 7:
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

Bloc 8:
Commentaire :
Cette fonction met à jour la grille pour y placer un bateau.
Elle reçoit :
- grid : la grille (t_grid),
- positions : la liste des positions que le bateau occupera.
Pour chaque position, la valeur de la case est mise à 1.
La fonction est récursive et parcourt la liste des positions.
@author TOTSKYI Hlib
@author TERRENOIRE Yvan

Bloc 9:
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

Bloc 10:
Commentaire :
Cette fonction calcule la position en pixels du coin inférieur gauche d'une cellule.
Elle reçoit :
- x0, y0 : les coordonnées du coin inférieur gauche de la grille,
- i, j : les indices (colonne, ligne) de la cellule,
- cell_size : la taille d'une cellule en pixels.
Le résultat est un couple (x, y) indiquant la position en pixels.
@author TOTSKYI Hlib
@author AHAMADI Izaki

Bloc 11:
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

Bloc 12:
Commentaire :
Cette version de display_grid_color parcourt la grille et colore les cases en fonction de leur état :
- 0 pour l'eau (pas de couleur),
- 1 pour bateau non touché (pas de couleur),
- 2 pour tir manqué (vert),
- 3 pour bateau touché (orange),
- 4 pour bateau coulé (rouge).
@author TOTSKYI Hlib

Bloc 13:
Commentaire :
Cette fonction affiche une liste de messages dans la zone réservée aux messages.
Elle reçoit :
- msgs : la liste des chaînes de caractères à afficher,
- params : la structure t_params (pour obtenir margin et message_size),
- win_width : la largeur totale de la fenêtre.
Elle efface d'abord la zone de messages en la remplissant en blanc, puis affiche chaque ligne de message en noir.
La fonction auxiliaire aux (récursive) permet d'afficher chaque message sur une ligne distincte.
@author TOTSKYI Hlib

Bloc 14:
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

Bloc 15:
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

Bloc 16:
Commentaire :
exists vérifie récursivement si un élément x existe dans la liste lst.
Le paramètre x est polymorphe ('a) pour être compatible avec tout type.
@author TOTSKYI Hlib

Bloc 17:
Commentaire :
all_listes vérifie si la fonction f est vraie pour tous les éléments de la liste lst.
@author TOTSKYI Hlib

Bloc 18:
Commentaire :
appl applique une fonction f à chaque élément de la liste lst, en utilisant la récursion.
@author TOTSKYI Hlib

Bloc 19:
Commentaire :
check_cell retourne true si la case (i, j) dans grid a la valeur 3 (bateau touché).
@author AHAMADI Izaki

Bloc 20:
check_sunk_ship retourne true si toutes les positions du bateau sont à 3 dans grid, indiquant qu'il est coulé.
@author TERRENOIRE Yvan

Bloc 21:
Commentaire :
set_cell modifie la case correspondant à pos dans grid en lui assignant 4, indiquant que le bateau est coulé.
@author AHAMADI Izaki

Bloc 22:
Commentaire :
sink_ship parcourt la liste des positions du bateau et met à jour chacune avec set_cell.
@author TERRENOIRE Yvan

Bloc 23:
Commentaire :
update_grid modifie la valeur d'une cellule en fonction du tir :
- Si la cellule est vide (0), elle devient 2 (tir manqué).
- Si elle contient un bateau (1), elle devient 3 (bateau touché).
@author TERRENOIRE Yvan
@author AHAMADI Izaki

Bloc 24:
Commentaire :
find_ship recherche dans la liste des bateaux le bateau dont les positions contiennent pos.
Si aucun bateau n'est trouvé, un bateau "ship" est retourné.
@author TOTSKYI Hlib

Bloc 25:
Commentaire :
player_shoot permet au joueur de tirer sur la grille.  
Elle lit la position du clic via read_mouse et, selon la valeur de la cellule ciblée :
- Met à jour la cellule (0 → 2 ou 1 → 3) avec update_grid,
- Si le tir touche un bateau (la cellule passe à 3), elle recherche le bateau via find_ship, 
vérifie s'il est coulé avec check_sunk_ship et, le cas échéant, applique sink_ship.
La fonction retourne la liste des bateaux sans modification.
@author TOTSKYI Hlib

Bloc 26:
Commentaire :
player_turns permet d'effectuer n tirs successifs du joueur.
Après chaque tir, la grille est redessinée pour refléter les changements (tir manqué, touché ou coulé).
@author TOTSKYI Hlib

Bloc 27:
Commentaire :
check_cell_x retourne true si la cellule (i, j) vaut 4, c'est-à-dire si le bateau est coulé.
@author TERRENOIRE Yvan

Bloc 28:
Commentaire :
ship_sunk vérifie que toutes les cellules occupées par le bateau sont à 4, indiquant qu'il est coulé.
@author AHAMADI Izaki

Bloc 29:
Commentaire :
all_sunk retourne true si tous les bateaux de la liste sont coulés (toutes leurs cellules valent 4).
@author AHAMADI Izaki
@author TERRENOIRE Yvan

Bloc 30:
Commentaire :
computer_shoot permet à l'ordinateur de tirer aléatoirement sur la grille.
Si la cellule choisie a déjà été tirée, il recommence.
Si le tir touche un bateau (cellule passe à 3), il cherche le bateau via find_ship, vérifie s'il est coulé,
et le marque comme coulé avec sink_ship si nécessaire.
@author TOTSKYI Hlib

Bloc 31:
Commentaire :
Ce type structuré regroupe les éléments essentiels du jeu :
- comp_grid : la grille de l'ordinateur (t_grid),
- player_grid : la grille du joueur (t_grid),
- comp_ships : la liste des bateaux placés pour l'ordinateur (t_ship list),
- player_ships : la liste des bateaux placés pour le joueur (t_ship list).
Il sert à stocker l'état initial du jeu après le placement des bateaux.
@author TOTSKYI Hlib

Bloc 32:
Commentaire :
all_shoot alterne les tirs du joueur et de l'ordinateur jusqu'à ce que tous les bateaux de l'une des parties soient coulés.
Si tous les bateaux de l'ordinateur sont coulés, le message "Vous avez gagné !" s'affiche.
Si tous les bateaux du joueur sont coulés, le message "Vous avez perdu !" s'affiche.
Sinon, on effectue un tour de tirs pour chaque côté et on recommence récursivement.
@author TOTSKYI Hlib

Bloc 33:
Commentaire :
Cette fonction initialise l'état du jeu (t_battleship).
Elle réalise les étapes suivantes :
- Crée deux grilles (une pour l'ordinateur et une pour le joueur) à l'aide de init_grid.
- Place automatiquement les bateaux sur la grille de l'ordinateur avec auto_placing_ships.
- Permet au joueur de placer manuellement ses bateaux sur sa grille via manual_placing_ship_list.
Le résultat est un enregistrement de type t_battleship contenant les grilles et les listes de bateaux placés.
@author TOTSKYI Hlib

Bloc 34:
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

============================================================
Fin de la documentation.
