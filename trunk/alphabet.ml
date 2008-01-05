open Ajourbiais
open Affichage
let lgr=Array.make 26 0;;
let queues = Array.make 26 (Queue.create ());;

let int_to_ascii_string i = String.make 1 (char_of_int i);;

for i=0 to 25 do
queues.(i) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.spectre ("./alphabet/"^int_to_ascii_string(i+97)^".wav"));
lgr.(i) <- Queue.length queues.(i)
done;;

let res=generation [|20;10;26|] 80;;

let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;

let j=ref 0 in
for i=0 to 25 do
	let fin = !j + lgr.(i) in
  while !j < fin do
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init 26 (fun k -> if k=i then 0.95 else 0.05);
	 incr j
  done
done;;

let tmp=super_train_log_eta res tab_couples 0.8 (600*100) sigmoide;;

let (_,l2)=tmp in affiche "./results/alphabet_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/alphabet_struct";;
