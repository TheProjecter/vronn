exception Trouve

open Reseaux
open Affichage
open Params

let lgr=Array.make nb_files 0
let queues = Array.make nb_files (Queue.create ());;

for i=0 to nb_files-1 do
  queues.(i) <- Fft.queue_map (fun tab -> Array.sub (Fft.array_of_res_norm_moy norme moy tab) debut_tab_fft taille_tab_fft) (Fft.spectre ("./midge/"^files.(i)^".wav") dt);
  lgr.(i) <- Queue.length queues.(i)
done;;

print_endline ("Done with the Fastest Fourier Transform in the West ! (" ^ (string_of_int nb_files) ^")" );;

let compose elem str =
	try
		for i=0 to (String.length str) / 3 do
			if String.sub str (i * 3) 2 = elem then raise Trouve
		done; 
		false
	with |Trouve -> true;;


let tri elems j = 
	if List.mem j elems then 0.95 else 0.05
	
let res=(*generation [|(*160;*)80;40;nb_simples|] (Array.length (Queue.peek queues.(0)));;*)
 load_struct "./results/notes_struct";;

let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;
let j=ref 0 in
for i=0 to nb_files-1 do
	let fin = !j + lgr.(i) in
	if i< nb_simples 
	then 
	 (while !j < fin do
			tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_simples (tri [i]);
			incr j
		done)
	else 
	 (let lst = ref [] in 
		for k=0 to nb_simples -1 do
			if compose (String.sub files.(k) 0 2) files.(i) then lst:=k::(!lst)
		done;
		while !j < fin do
			tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_simples (tri !lst);
			incr j
		done);
done;;

let tmp=super_train_log_eta res tab_couples 0.01 (6000);;

let (_,l2)=tmp in if List.length l2 < 800 then affiche "./results/notes_erreur" (List.rev l2) else Printf.printf "Trop d'erreurs à afficher...\n";;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/notes_struct";;
