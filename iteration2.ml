(*@author TOTSKYI Hlib
@author TERRENOIRE Yvan
@author AHAMADI Izaki*)

(*Iteration 2*)

(*1*)
type t_ship_size = {nom: string; len: int}
;;

type t_params = {
  margin : int;        
  cell_size : int;     
  message_size : int;  
  grid_size : int;     
  window_width : int;  
  window_height : int; 
  ship_sizes: t_ship_size list ;
}
;;

type t_grid = sting array array
;;

type t_ship = {name0: string, pos: (int * int) list}
;;
