open Tipedom

(*xor avec biais*)
let res=generation [|3;1|] 2;;
let tab_couples=[| ([|1.;1.|],[|0.05|] );([|0.;0.95|],[|0.95|] );([|1.;0.|],[|0.95|] );([|0.;0.|],[|0.05|] ); |];;

Printf.printf "Le Xor biaise :) \n\n";;

Sys.catch_break true;

(try
ignore (super_train_log res  tab_couples 0.5 300000 sigmoide)
with
|Sys.Break -> ());;

Printf.printf "erreur : %f \n poids : " (erreur res tab_couples);
for i=0 to 1 do
	for j=0 to Array.length res.(i) -1 do
		for k=0 to Array.length res.(i).(j).poids -1 do
			Printf.printf "%f," res.(i).(j).poids.(k)
		done;
		Printf.printf ";";
	done;
	Printf.printf "\n";
done
