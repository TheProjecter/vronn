print_endline "Exécution de la fft sur a.wav...";;
let queue_a = Fft.main "../alphabet/a.wav";;
print_endline "Exécution de la fft sur b.wav...";;
let queue_b = Fft.main "../alphabet/b.wav";;
print_endline "Ffts effectuées.";;
print_endline "Apprentissage en cours...";;
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

let tmp=super_train_log_eta res tab_couples 0.5 (600*100) sigmoide;;

let (_,l2)=tmp in affiche "../results/alphabet_erreur" (List.rev l2);;

(*test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (erreur res tab_couples);;

save_struct res "../results/alphabet_struct";;
