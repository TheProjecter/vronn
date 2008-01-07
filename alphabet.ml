open Ajourbiais
open Affichage
let nb_lettres = 4;;
let lgr=Array.make nb_lettres 0;;
let queues = Array.make nb_lettres (Queue.create ());;

let int_to_ascii_string i = String.make 1 (char_of_int i);;

let dirhandle=Unix.opendir "alphabet2" in
for i=1 to 3 do ignore (Unix.readdir dirhandle); done;
for i=0 to nb_lettres-1 do
	let filename=Unix.readdir dirhandle in Printf.printf "alphabet2/%s\n" filename;
	queues.(i) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.spectre ("./alphabet/"^filename));
	lgr.(i) <- Queue.length queues.(i)
done;
Unix.closedir dirhandle;;

let res=generation [|10;10;nb_lettres|] 80;;

let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;

let j=ref 0 in
for i=0 to nb_lettres-1 do
	let fin = !j + lgr.(i) in
  while !j < fin do
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_lettres (fun k -> if k=i then 0.95 else 0.05);
	 incr j
  done
done;;

let tmp=super_train_log_eta res tab_couples 0.0001 (600*100) sigmoide;;

let (_,l2)=tmp in affiche "./results/alphabet_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/alphabet_struct";;
