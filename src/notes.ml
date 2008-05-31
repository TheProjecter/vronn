exception Trouve

open Reseaux
open Affichage
open Params

let lgr=Array.make nb_files 0
let queues = Array.make nb_files (Queue.create ());;

for i=0 to nb_files-1 do
  queues.(i) <- Fft.queue_map (fun tab -> Array.sub (Fft.array_of_res_norm_moy
    norme moy tab) debut_tab_fft taille_tab_fft) (Fft.spectre ("../midge/"^files.(i)^".wav") dt);
	let tmp = Queue.create () in
	for j=0 to Queue.length queues.(i) / div_sample_per_file do
		Queue.push (Queue.pop queues.(i)) tmp
	done;
	queues.(i) <- tmp;
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
	
let res = 
	if reload_res then load_struct "./results/notes_struct"
	else generation taille_res (Array.length (Queue.peek queues.(0)));;

let tab_couples=Array.make (Array.fold_left (+) 0 lgr) ([||],[||]);;

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


let tcio = Unix.tcgetattr Unix.stdin;;
try
	Unix.tcsetattr Unix.stdin  Unix.TCSADRAIN ({tcio with Unix.c_vmin=1; Unix.c_icanon=false});
	let l2 = 
		if progressive_training then (
			let a=ref [] in
			try let nentrainement=ref 2 in
			while !nentrainement < nb_files do
				a:=!a@ (snd (super_train_log_eta res (Array.sub tab_couples 0 (Array.fold_left (+) 0 (Array.sub lgr 0 !nentrainement))) (0.02) 6000 0.5));
				nentrainement:=!nentrainement+nb_files/4
			done;
			a:=!a@(snd (super_train_log_eta res tab_couples 0.001 6000 0.4));
			!a
			with Sys.Break -> !a)
		else snd (super_train_log_eta res tab_couples 0.005 6000 0.2)
	in if List.length l2 < 800 then affiche "./results/notes_erreur" (List.rev l2) else Printf.printf "Trop d'erreurs à afficher...\n"
with |z -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio; raise z;;
(* test res tab_couples;;*)


save_struct res "./results/notes_struct";;
