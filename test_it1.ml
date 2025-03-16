let test_iter1_structure () : unit =
  let params : t_params = init_params () in
  assert (params.margin = 30);
  assert (params.cell_size = 15);
  assert (params.message_size = 60);
  assert (params.grid_size = 10);
  assert (params.window_width = 30 + (15 * 10) + 30 + (15 * 10) + 30);
  assert (params.window_height = 30 + 60 + (15 * 10) + 30);
  Printf.printf "Test itération 1 (structure) : OK\n"
;;
(*test de structure pour l'itération 1 : vérifie que init_params retourne les bonnes valeurs
@author TOTSKYI Hlib
@author TERRENOIRE Yvan*)

let test_iter1_fonctionnel () : unit =
  Printf.printf "\nTest itération 1 (fonctionnel) :\n";
  Printf.printf "Vérifiez que l'affichage ci-dessous correspond à deux grilles vides\n";
  Printf.printf "côte à côte avec les étiquettes 'Ordinateur' et 'Joueur'.\n\n";
  display_empty_grids (init_params ());
  Printf.printf "\nTest itération 1 (fonctionnel) : Vérification manuelle terminée.\n"
;;
(*test fonctionnel pour l'itération 1 : vérifie l'affichage des grilles vides
@author TOTSKYI Hlib
@author AHAMADI Izaki*)

let () : unit =
  test_iter1_structure ();
  test_iter1_fonctionnel ()
;;
(*exécution des tests pour l'itération 1*)