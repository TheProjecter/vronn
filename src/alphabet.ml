open Ajourbiais
open Affichage
let nb_lettres = 42;;
let lgr=Array.make nb_lettres 0;;
let queues = Array.make nb_lettres (Queue.create ());;

let int_to_ascii_string i = String.make 1 (char_of_int i);;

let files = Sys.readdir "alphabet2" ;;
Array.sort compare files;
for i=0 to nb_lettres-1 do
 queues.(i) <- Fft.queue_map (Fft.re_array_of_cplx_bigarray1_norm 8800000.) (Fft.cepstre ("./alphabet2/"^files.(i+1)));
  lgr.(i) <- Queue.length queues.(i)
done;;

let distrib=[|"cons";"voy";"autres";"batt";"clic";"ejec";"expo";"fric";"inje";"latefric";"late";"nasa";"occl";"roul";"spir";"ante";"arri";"cent";"conti";"anterieures";"arrieres";"centrales";"fermees";"mifermees";"miouvertes";"ouvertes";"arrondies";"tendues"|];;
let nb_tri = Array.length distrib;;
let res=generation [|10;10;nb_tri|] 110;;
let tri j k=
(*fun int->float*)
  let recherche str str2=
    let i=ref 0 and go_on=ref true in
    while !go_on && !i<(String.length str) - (String.length str2) do
      if String.sub str !i (String.length str2)=str2 then go_on:=false;
    incr i    
    done;
    not !go_on in
  if k<nb_tri && k>0 
    then if recherche files.(j) distrib.(k) then 0.95 else 0.05
    else 0.05
  
;;



let tab_couples=Array.make (Array.fold_left (fun x y -> x+y) 0 lgr) ([||],[||]);;
let j=ref 0 in
for i=0 to nb_lettres-1 do
  let fin = !j + lgr.(i) in
  while !j < fin do
   tab_couples.(!j) <- Queue.pop queues.(i),Array.init 28 (tri i);
   incr j
  done
done;;

let tmp=super_train_log_eta res tab_couples 0.000001 (600*100);;

let (_,l2)=tmp in affiche "./results/alphabet_erreur" (List.rev l2);;

(* test res tab_couples;;*)

Printf.printf "Erreur %f \n\n" (super_erreur res tab_couples);;

save_struct res "./results/alphabet_struct";;
