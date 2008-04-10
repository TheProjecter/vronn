exception Trouve

open Reseaux
open Affichage
let gammes = (*[|"3"; "4"|]*) [|"3";"4";"5";"6"|]
let notes = (*[|"c";"e";"g";"a"|]*) [|"c";"d";"e";"f";"g";"a";"b"|]
let nb_notes = Array.length notes
let nb_simples = nb_notes * Array.length gammes
let nb_couples = (*nb_simples * 2 - 3 *) nb_simples * (nb_simples - 1) / 2 
let simples = Array.init nb_simples (fun i -> gammes.(i / nb_notes) ^ notes.(i mod nb_notes))
let couples = 
	let t= Array.make nb_couples "" and c = ref 0 in
	(try 
	for i = 0 to nb_simples-1 do
		for j = i+1 to nb_simples-1 do
			t.(!c) <- simples.(i) ^ "," ^ simples.(j); incr c; if !c >= nb_couples then failwith "fin"
		done;
	done;
	with Failure "fin" -> ());
	t

let nb_files = nb_simples + nb_couples
let files = Array.concat [simples; couples]

let lgr=Array.make nb_files 0
let queues = Array.make nb_files (Queue.create ());;

for i=0 to nb_files-1 do
  queues.(i) <- Fft.queue_map (Fft.array_of_res_norm_moy 8800000. 3) (Fft.spectre ("./midge/"^files.(i)^".wav"));
	queues.(i) <- Fft.queue_map (fun tab -> (*Array.map (fun x -> if x < 1. then 0. else if x < 10. then 0.5 else if x < 100. then 1. else 2.)*) (Array.sub tab 0 500)) queues.(i);
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
	
let res=generation [|160;80;40;nb_simples|] (Array.length (Queue.peek queues.(0)));;
(* load_struct "./results/notes_struct";; *)

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
