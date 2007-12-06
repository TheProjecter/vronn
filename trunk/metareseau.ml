open Ajourbiais

let bob=Array.make_matrix 10 10 0.;;
let tab_couples=[|
([|0.44|],[|0.95;0.1;0.1;0.1;0.1;0.1|] );  (* la *)
([|0.22|],[|0.95;0.1;0.1;0.1;0.1;0.1|] );  (* la2 *)
([|0.495|],[|0.1;0.95;0.1;0.1;0.1;0.1|] );  (* si*)
([|0.523|],[|0.1;0.1;0.95;0.1;0.1;0.1|] );  (* do *)
([|0.588|],[|0.1;0.1;0.1;0.95;0.1;0.1|] );  (* ré *)
([|0.294|],[|0.1;0.1;0.1;0.95;0.1;0.1|] );  (* ré *)
([|0.33|],[|0.1;0.1;0.1;0.1;0.95;0.1|] );  (* mi *)
([|0.66|],[|0.1;0.1;0.1;0.1;0.95;0.1|] );  (* mi *)
([|0.699|],[|0.1;0.1;0.1;0.1;0.1;0.95|] );  (* fa *)
([|0.350|],[|0.1;0.1;0.1;0.1;0.1;0.95|] )  (* fa *)
|];;


for i=5 to 10 do
	for j=5 to 10 do
		let res= generation [|i;j;6 |] 1 in
		let _,l2 = super_train_log res  tab_couples 0.5 30 sigmoide in
		bob.(i-1).(j-1) <- super_erreur res tab_couples ;
		Printf.printf " --------------- WHEEEE ! %d, %d done -------------- \n\n\n" i j;
		Affichage.affiche "onsenfout" (List.rev l2)
	done
done;;

let troiz (_,_,z) = z;;

let min1,min2,min3 = ref (0,0,1.), ref (0,0,1.), ref (0,0,1.) in
for i=0 to 9 do
	for j=0 to 9 do
		if bob.(i).(j) < troiz !min1 then
			begin
				min3 := !min2; min2:= !min1; min1 := (i+1,j+1,bob.(i).(j))
			end
else if bob.(i).(j) < troiz !min2 then 	begin
				min3 := !min2; min2:= (i+1,j+1,bob.(i).(j))
			end else if bob.(i).(j) < troiz !min3 then 	begin
			 min3:= (i+1,j+1,bob.(i).(j))
			end
	done
done;

let print_tr (a,b,c) =Printf.printf "%d, %d, avec pour erreur %f\n" a b c in

print_tr !min1; print_tr !min2; print_tr !min3
