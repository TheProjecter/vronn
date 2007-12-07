
let queue_a = Fft.main "/root/localWork/tipe/alphabet/a.wav";;
let queue_b = Fft.main "/root/localWork/tipe/alphabet/b.wav";;
let lgr_a = Queue.length queue_a and lgr_b = Queue.length queue_b;;
let lgr = lgr_a + lgr_b;;

open Ajourbiais
open Affichage

let res=generation [|10;2|] 80;;

let tab_couples=Array.make lgr ([||],[||]);;
for i=0 to lgr_a-1 do
tab_couples.(i) <- Queue.pop queue_a,[|0.95; 0.1|];
(*	ignore (Queue.pop queue); *)
done;
for i=0 to lgr_b-1 do
tab_couples.(i+lgr_a) <- Queue.pop queue_b,[|0.1; 0.95|];
(*	ignore (Queue.pop queue); *)
done;;

let tmp=super_train_log res tab_couples 0.5 100 sigmoide;;

let (_,l2)=tmp in affiche "results/alphabet_erreur" (List.rev l2);;

test res tab_couples;;

Printf.printf "Erreur %f \n\n" (erreur res tab_couples);;

save_struct res "results/alphabet_struct";;
