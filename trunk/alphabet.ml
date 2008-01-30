open Reseaux
open Affichage
let nb_lettres = 1 ;;
let lgr=Array.make nb_lettres 0;;
let queues = Array.make nb_lettres (Queue.create ());;


 queues.(0) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.spectre ("./test2.wav"));
  lgr.(0) <- Queue.length queues.(0)


let res=generation [|10;10;2|] 110;;

let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;
let j=ref 0 in
for i=0 to nb_lettres-1 do
	let fin = !j + lgr.(i) in
  while !j < fin do
(* Printf.printf "%d\n" !j;
		flush stdout; *)
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init 2 (fun i -> if i=0 then 0.95 else 0.05);
	 incr j
  done
done;;

let tmp=super_train_log_eta res tab_couples 0.000001 (10) sigmoide;;

let (_,l2)=tmp in affiche "./results/alphabet_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/alphabet_struct";;
