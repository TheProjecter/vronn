let bob=Array.make_matrix 15 15 0.;;

let queue_a = Fft.main "../alphabet/a.wav";;
let queue_b = Fft.main "../alphabet/b.wav";;
let queue_c = Fft.main "../alphabet/c.wav";;
let lgr_a = Queue.length queue_a and lgr_b = Queue.length queue_b and lgr_c=Queue.length queue_c;;
let lgr = lgr_a + lgr_b + lgr_c;;

open Ajourbiais
open Affichage

let tab_couples=Array.make lgr ([||],[||]);;
for i=0 to lgr_a-1 do
	tab_couples.(i) <- Queue.pop queue_a,[|0.95; 0.1; 0.1|];
	(*  ignore (Queue.pop queue); *)
done;
for i=0 to lgr_b-1 do
	tab_couples.(i+lgr_a) <- Queue.pop queue_b,[|0.1; 0.95; 0.1|];
	(*  ignore (Queue.pop queue); *)
done;
for i=0 to lgr_c-1 do
	tab_couples.(i+lgr_a+lgr_b) <- Queue.pop queue_c,[|0.1; 0.1; 0.95|];
done;;

for i=1 to 15 do
  for j=1 to 15 do
    let res= generation [|i;j;2 |] 80 in
    let _,l2 = super_train_log res  tab_couples 0.5 2000 sigmoide in
    bob.(i-1).(j-1) <- super_erreur res tab_couples ;
    Printf.printf " --------------- WHEEEE ! %d, %d done -------------- \n\n\n" i j;
    Affichage.affiche ("../results/alphabet" ^ (string_of_int i) ^","^ (string_of_int j)) (List.rev l2);
		save_struct res ("../results/alphabet" ^ (string_of_int i) ^","^ (string_of_int j) ^"_struct")
  done
done;;

let troiz (_,_,z) = z;;

let min1,min2,min3 = ref (0,0,1.), ref (0,0,1.), ref (0,0,1.) in
for i=0 to 14 do
  for j=0 to 14 do
    if bob.(i).(j) < troiz !min1 then
      begin
        min3 := !min2; min2:= !min1; min1 := (i+1,j+1,bob.(i).(j))
      end
		else if bob.(i).(j) < troiz !min2 then  begin
        min3 := !min2; min2:= (i+1,j+1,bob.(i).(j))
    end else if bob.(i).(j) < troiz !min3 then  begin
       min3:= (i+1,j+1,bob.(i).(j))
    end
  done
done;

let print_tr (a,b,c) =Printf.printf "%d, %d, avec pour erreur %f\n" a b c in

print_tr !min1; print_tr !min2; print_tr !min3
