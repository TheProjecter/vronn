let queue = Fft.main "dodolala.wav";;

open Ajourbiais
open Affichage

let res=generation [|10;2|] 220;;

let tab_couples=Array.make 341 ([||],[||]);;
for i=0 to 170 do
tab_couples.(i) <- Queue.pop queue,[|0.95; 0.1|];
ignore (Queue.pop queue);
done;
for i=171 to 340 do
tab_couples.(i) <- Queue.pop queue,[|0.1; 0.95|];
ignore (Queue.pop queue);
done;;

let tmp=super_train_log res tab_couples 0.5 100 sigmoide;;

let (_,l2)=tmp in affiche "dodolala" (List.rev l2);;

test res tab_couples;;

Printf.printf "Erreur %f \n\n" (erreur res tab_couples);;

save_struct res "struct_dodolala";;
