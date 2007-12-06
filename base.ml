open Ajourbiais
open Affichage

(*nom de lessai*)
let res=generation [|5;10;6|] 1;; (*structure du reseau*)

let tab_couples=[|
([|0.44|],[|0.95;|] );  (* exemple *)

|];;


Printf.printf "Nom du Test \n\n";;


let tmp=super_train_log res  tab_couples 0.5 30 sigmoide;;
let (_,l2) = tmp in
affiche "nom_de_limage" (List.rev l2);;

test res tab_couples;;
Printf.printf "Erreur %f\n\n" (erreur res tab_couples);;
save_struct res "struct_du_test";;
let res2=load_struct "struct_du_test" in
test res2 tab_couples;;
