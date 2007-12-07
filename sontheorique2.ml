open Ajourbiais
open Affichage

(*son theorique avec les frequences*)
let res=generation [|5;10;6|] 1;;

let tab_couples=[|
([|0.44|],[|0.95;0.1;0.1;0.1;0.1;0.1|] );  (* la *)
([|0.22|],[|0.95;0.1;0.1;0.1;0.1;0.1|] );  (* la2 *)
([|0.495|],[|0.1;0.95;0.1;0.1;0.1;0.1|] );  (* si*)
([|0.523|],[|0.1;0.1;0.95;0.1;0.1;0.1|] );  (* do *)
([|0.588|],[|0.1;0.1;0.1;0.95;0.1;0.1|] );  (* ré *)
([|0.294|],[|0.1;0.1;0.1;0.95;0.1;0.1|] );  (* ré *)
([|0.33|],[|0.1;0.1;0.1;0.1;0.95;0.1|] );  (* mi *)
([|0.66|],[|0.1;0.1;0.1;0.1;0.95;0.1|] );  (* mi *)
([|0.699|],[|0.1;0.1;0.1;0.1;0.1;0.95|] );  (* fa *)
([|0.350|],[|0.1;0.1;0.1;0.1;0.1;0.95|] )  (* fa *)
|];;


Printf.printf "Le Son theorqiue \n\n";;


let tmp=super_train_log_eta res  tab_couples 0.5 30 sigmoide;;
let (_,l2) = tmp in
affiche "resultsonth2" (List.rev l2);;

test res tab_couples;;
Printf.printf "Erreur %f\n\n" (erreur res tab_couples);;
save_struct res "struct_sontheorique2";;
let res2=load_struct "struct_sontheorique2" in
(*test res2 tab_couples;;*)
Printf.printf "Erreur de la sauvegarde %f\n\n" (erreur res2 tab_couples);;
