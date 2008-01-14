open Ajourbiais
open Affichage
let nb_lettres = 10;;
let lgr=Array.make nb_lettres 0;;
let queues = Array.make nb_lettres (Queue.create ());;

let int_to_ascii_string i = String.make 1 (char_of_int i);;


(*---version greg*)
(*let files = Sys.readdir "alphabet2" ;;
Array.sort compare files;
for i=0 to nb_lettres-1 do
 queues.(i) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.spectre ("./alphabet2/"^files.(i+1)));
  lgr.(i) <- Queue.length queues.(i)
done;;

let res=generation [|10;10;nb_lettres|] 110;;

let super_mega_fonction_de_greg j k=
(*fun int->float*)
	let recherche str str2=
		let i=ref 0 and go_on=ref true in
		while !go_on && !i<(String.length str) - (String.length str2) do
			if String.sub str !i (String.length str2)=str2 then go_on:=false
		done;
		not !go_on in
	if k=1 then if recherche files.(j) "cons" then 0.95 else 0.05
	else if k=2 then if recherche files.(j) "voy" then 0.95 else 0.05
	else if k=3 then if recherche files.(j) "autres" then 0.95 else 0.05
	else if k=4 then if recherche files.(j) "batt" then 0.95 else 0.05
	else if k=5 then if recherche files.(j) "clic" then 0.95 else 0.05
	else if k=6 then if recherche files.(j) "ejec" then 0.95 else 0.05
	else if k=7 then if recherche files.(j) "expo" then 0.95 else 0.05
	else if k=8 then if recherche files.(j) "fric" then 0.95 else 0.05
	else if k=9 then if recherche files.(j) "inje" then 0.95 else 0.05
	else if k=10 then if recherche files.(j) "latefric" then 0.95 else 0.05
	else if k=11 then if recherche files.(j) "late" then 0.95 else 0.05
	else if k=12 then if recherche files.(j) "nasa" then 0.95 else 0.05
	else if k=13 then if recherche files.(j) "occl" then 0.95 else 0.05
	else if k=14 then if recherche files.(j) "roul" then 0.95 else 0.05
	else if k=15 then if recherche files.(j) "spir" then 0.95 else 0.05
	else if k=16 then if recherche files.(j) "ante" then 0.95 else 0.05
	else if k=17 then if recherche files.(j) "arri" then 0.95 else 0.05
	else if k=18 then if recherche files.(j) "cent" then 0.95 else 0.05
	else if k=19 then if recherche files.(j) "conti" then 0.95 else 0.05
	else if k=20 then if recherche files.(j) "anterieures" then 0.95 else 0.05
	else if k=21 then if recherche files.(j) "arrieres" then 0.95 else 0.05
	else if k=22 then if recherche files.(j) "centrales" then 0.95 else 0.05
	else if k=23 then if recherche files.(j) "fermees" then 0.95 else 0.05
	else if k=24 then if recherche files.(j) "mifermees" then 0.95 else 0.05
	else if k=25 then if recherche files.(j) "miouvertes" then 0.95 else 0.05
	else if k=26 then if recherche files.(j) "ouvertes" then 0.95 else 0.05
	else if k=27 then if recherche files.(j) "arrondies" then 0.95 else 0.05
	else if k=28 then if recherche files.(j) "tendues" then 0.95 else 0.05
;;




let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;

let j=ref 0 in
for i=0 to nb_lettres-1 do
	let fin = !j + lgr.(i) in
  while !j < fin do
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_lettres (super_mega_fonction_de_greg i);
	 incr j
  done
done;;*)
(*--fin version greg--*)




let files = Sys.readdir "alphabet2" in
Array.sort compare files;
for i=0 to nb_lettres-1 do
 queues.(i) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.spectre ("./alphabet2/"^files.(i+1)));
  lgr.(i) <- Queue.length queues.(i)
done;;

let res=generation [|10;10;nb_lettres|] 110;;


let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;

let j=ref 0 in
for i=0 to nb_lettres-1 do
	let fin = !j + lgr.(i) in
  while !j < fin do
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init nb_lettres (fun k -> if k=i then 0.95 else 0.05);
	 incr j
  done
done;;

let tmp=super_train_log_eta res tab_couples 0.000001 (600*100) sigmoide;;

let (_,l2)=tmp in affiche "./results/alphabet_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/alphabet_struct";;
