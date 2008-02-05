open Reseaux
open Affichage
let nb_notes = 9;;
let nb_tri = 7;;
let lgr=Array.make nb_notes 0;;
let queues = Array.make nb_notes (Queue.create ());;

let files = Sys.readdir "temp" ;;
let ordre str1 str2 = match compare (String.length str1) (String.length str2) with
	|0 -> compare str1 str2
	|i -> i
in
Array.sort ordre files;
for i=0 to nb_notes-1 do
  queues.(i) <- Fft.queue_map (Fft.array_of_res_norm_moy 880000000. 20) (Fft.spectre ("./temp/"^files.(i+1)));
	let n= Queue.length queues.(i) in
	for j = 0 to n / 4 do ignore (Queue.pop queues.(i)) done;
	let temp = Queue.create () in
	for j = 0 to n / 2 do Queue.push (Queue.pop queues.(i)) temp done;
	queues.(i) <- temp;
  lgr.(i) <- Queue.length queues.(i)
done;;

print_endline ("Done with the Fastest Fourier Transform in the West ! (" ^ (string_of_int nb_notes) ^")" );;

exception Trouve;;

let compose elem str =
	try
		for i=0 to (String.length str) / 4 - 1 do
			if String.sub str (i * 4) 3 = elem then raise Trouve
		done; 
		false
	with |Trouve -> true;;


let tri elems j = 
	if List.mem j elems then 0.95 else 0.05
	
let res=generation [|10;8;nb_tri|] (Array.length (Queue.peek queues.(0)));;

let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;
let j=ref 0 in
for i=0 to nb_notes-1 do
	let fin = !j + lgr.(i) in
	if i< nb_tri 
	then 
	 (while !j < fin do
			tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_tri (tri [i]);
			incr j
		done)
	else 
	 (let lst = ref [] in 
		for k=0 to nb_tri -1 do
			if compose (String.sub files.(k+1) 0 3) files.(i+1) then lst:=k::(!lst)
		done;
		while !j < fin do
			tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_tri (tri !lst);
			incr j
		done);
done;;


let tmp=super_train_log_eta res tab_couples 0.01 (6000) sigmoide;;

let (_,l2)=tmp in affiche "./results/notes_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/notes_struct";;
