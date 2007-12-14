Gc.set {(Gc.get()) with Gc.minor_heap_size=200000};;

Random.init (int_of_float (Unix.time ()));;

type neurone=
		{poids: float array; mutable activation:float; mutable sortie:float;mutable sensib: float};;
type reseau=neurone array array;;

let t=ref 0.;;
let scan_int () = Scanf.scanf " %d" (fun x -> x);;
let sigmoide =fun x->1./. (1. +.exp (-.x));;
<<<<<<< .mine
let dsigm x=
  let exp_moins_x = exp (-.x) in
    exp_moins_x /. (1. +.  exp_moins_x) **2.;;
=======
let dsigm x=
  let exp_moins_x=exp (-.x) in
    exp_moins_x /. (1. +. exp_moins_x) ** 2.
;;
>>>>>>> .r25

let propagation entree (res:reseau) sigmoide=
	let m=Array.length res in
	for neur=0 to Array.length res.(0) -1 do
                t:=0.;
		for i=0 to Array.length entree -1 do
			t:=!t +. (res.(0).(neur).poids.(i) *. entree.(i));
		done;
		res.(0).(neur).activation <- !t -. res.(0).(neur).poids.(Array.length entree); (* rajout du biais dans la premiere couche *)
		res.(0).(neur).sortie <- sigmoide res.(0).(neur).activation
	done;
	for couche=1 to m-1 do
		for neur=0 to Array.length res.(couche) -1 do
                        t:=0.;
                        let couche_moins_un=couche-1 in
			for i=0 to Array.length res.(couche_moins_un) -1 do
				t:=!t+. (res.(couche).(neur).poids.(i) *.
                                res.(couche_moins_un).(i).sortie);
			done;
			res.(couche).(neur).activation <- !t -.
                          res.(couche).(neur).poids.(Array.length res.(couche_moins_un))  ;(*rajout du biais dans la couche*)
			res.(couche).(neur).sortie <- sigmoide res.(couche).(neur).activation
		done
	done
	;;

(*important !!*)

(*di = oi(1-oi)(ci-oi)
	cette formule vient d'une formule du calcul de la deriv�e dela sigmoide
	cf http://www.grappa.univ-lille3.fr/polys/apprentissage/sortie005.html#toc12 *)



let calcul_sensib (res:reseau) entree sortie eta=
  (*calcul de base*)
  let m=Array.length res in
  for neur=0 to Array.length res.(m-1) -1 do 
    res.(m-1).(neur).sensib <- (dsigm res.(m-1).(neur).activation) *. (sortie.(neur) -. res.(m-1).(neur).sortie)
  done;
  (*calcul recursif*)
  let couche=ref (m-2) and somme=ref 0. in
  while !couche>=0 do
    for neur=0 to Array.length res.(!couche) -1 do
      somme:=0.;
      let couche_plus_un= !couche+1 in
      for k=0 to Array.length res.(couche_plus_un) -1 do
        somme := !somme +. (res.(couche_plus_un).(k).sensib *. res.(couche_plus_un).(k).poids.(neur))
      done;
      res.(!couche).(neur).sensib <- (dsigm res.(!couche).(neur).activation) *. !somme;
      for k=0 to Array.length res.(couche_plus_un) -1 do (* modifie les poids de la couche*)
        let t=eta *. res.(couche_plus_un).(k).sensib in
        res.(couche_plus_un).(k).poids.(neur) <-
          res.(couche_plus_un).(k).poids.(neur) +. (t *. res.(!couche).(neur).sortie);
        res.(couche_plus_un).(k).poids.(Array.length res.(!couche)) <-
          res.(couche_plus_un).(k).poids.(Array.length res.(!couche)) -. t (*mise a jour du biais qui ne depend que de la sensib*);
      done;
    done;
    decr couche
  done;
  (*on modifie la premiere couche;  !couche=-1*)
  for input=0 to Array.length entree-1 do
    for k=0 to Array.length res.(0) -1 do
      let t=eta *. res.(0).(k).sensib in
      res.(0).(k).poids.(input) <- res.(0).(k).poids.(input) +. (t *. entree.(input) );
      res.(0).(k).poids.(Array.length entree) <-res.(0).(k).poids.(Array.length entree) -. t (*mise a jour du biais qui ne depend que de la sensib*);
    done;
  done
;;


let generation (nb_neurone_par_couche:int array) taille_entree=
	let nb_couche=Array.length nb_neurone_par_couche in
	let new_neur taille= 
		begin 
			let tmp=Array.make taille 0. in
			for i=0 to taille -1 do
				tmp.(i) <-   (Random.float 1. -. 0.5) 
			done;
			{poids=tmp ;activation=0. ;sortie=0. ; sensib= 0.}
		end in
	let reseau=Array.make nb_couche [||] in
	reseau.(0) <- 
		begin let tmp = Array.make nb_neurone_par_couche.(0)   (new_neur 0) in
					for j=0 to nb_neurone_par_couche.(0)- 1 do
						tmp.(j) <- new_neur (taille_entree+1) (*+1: a cause du biais*)
					done;
		tmp
		end;
	for couche=1 to nb_couche -1 do
		reseau.(couche) <- 
			begin let tmp = Array.make nb_neurone_par_couche.(couche) (new_neur 0) in
						for j=0 to nb_neurone_par_couche.(couche)-1 do
							tmp.(j) <- new_neur (nb_neurone_par_couche.(couche-1) +1) (*+1: a cause du biais*)
						done;
			tmp
			end;
	done;
	(reseau:reseau)
;;

let entrainement (reseau:reseau) tab_couples eta nb_test_max sigmoide=
	let n=Array.length tab_couples  in 
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	for i=0 to nb_test_max do
		p := fst tab_couples.(i mod n);
		d := snd tab_couples.(i mod n);
		propagation !p reseau sigmoide;
		calcul_sensib reseau !p !d eta;
	done
;;

let printf_tab tab=
	for i=0 to Array.length tab -1 do Printf.printf "%f " tab.(i) done;
	Printf.printf "\n"
;;

let test res tab_couples (* !! valable seulement pour un neurone sur la couche de sortie*)=
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	let n=Array.length tab_couples in
	let m=Array.length res in
	let bob=Array.make (Array.length res.(m-1) ) 0. in
	for i=0 to  n -1 do
		p := fst tab_couples.(i);
		d := snd tab_couples.(i);
		propagation !p res sigmoide;
		for k=0 to Array.length res.(m -1)  -1 do
			bob.(k) <- res.(m-1).(k).sortie
		done;
		Printf.printf "entrees        :";
		printf_tab !p;
		Printf.printf "sortie desiree :";
		printf_tab !d;
		Printf.printf "sortie reelles :";
		printf_tab bob;
		Printf.printf "\n----------------------------------\n";
	done;
;;

let erreur res tab_couples (* !! valable seulement pour un neurone sur la couche de sortie*)=
	let abs x=
		if x>0. then x else -.x in
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	let n=Array.length tab_couples in
	let erreur =ref 0. in
	for i=0 to  n -1 do
		p := fst tab_couples.(i);
		d := snd tab_couples.(i);
		propagation !p res sigmoide;
		erreur := !erreur +. (abs (!d.(0)-. res.(Array.length res -1).(0).sortie));
	done;
	!erreur /. (float_of_int n);
;;

let abs x=
	if x>0. then x else -.x;;

let super_erreur res tab_couples (* valable pour tout type de reseau*)=
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	let n=Array.length tab_couples in
	let erreur =ref 0. in
	let erreur2=ref 0. in
	for i=0 to  n -1 do
		p := fst tab_couples.(i);
		d := snd tab_couples.(i);
		propagation !p res sigmoide;
		for j=0 to Array.length (res.(Array.length res -1)) -1 do
			let tmp= abs (!d.(j)-. res.(Array.length res -1).(j).sortie)  in
			erreur := !erreur +. (tmp *. tmp);
		done;
		erreur2:=!erreur2 +. sqrt (!erreur);
		erreur:=0.;
	done;
	!erreur2 /. (float_of_int n);
;;

let train (res:reseau) tab_couples eta nb_test_max sigmoide=
let mistake=ref 0. in
let i=ref 0 in
while !i<= 100 && !mistake <> (erreur res tab_couples) do	
	mistake := erreur res tab_couples;
	Printf.printf "%f\n" (!mistake);
	entrainement res tab_couples eta (nb_test_max/100) sigmoide;
	incr i;
	done;
Printf.printf "au bout de: %d\n" (!i*nb_test_max/100);
;;

let anti_poids_nul res= (*permet de sauver les meubles si les poids deviennent nuls mais fait bcp remonter lerreur*)
for bob=0 to Array.length res -1 do
for neur=0 to Array.length res.(bob) -1 do
for w=0 to Array.length res.(bob).(neur).poids -1 do
if res.(bob).(neur).poids.(w) < 0.000000000001 then (res.(bob).(neur).poids.(w) <- (Random.float 1.) -. 0.5 ; Printf.printf "correction poids nul  faite\n")
done
done
done
;;




let super_train_log (res:reseau) tab_couples eta nb_test_max sigmoide=(*pseudo logarithmique, sert juste a mieux visualiser la pente initiale*)
  let pas=ref eta in
  let mistake=ref 1. in
  let last_erreur=ref 1. in
  let i=ref 0 in
  let go_on=ref true in
  let l1,l2=ref [],ref [] in
  Sys.catch_break true;
  let out_channel_disque=open_out "../results/fur_et_�_mesure" in
  try
    while !go_on && (*!i<= 1000000 &&*) !last_erreur > 0.001(*ceci est la borne sup
    des erreurs accept�es*) && !i < nb_test_max do
          (*Printf.printf "%f\n" (!mistake);*)
          entrainement res tab_couples !pas (10000) sigmoide;
          mistake := super_erreur res tab_couples;
          (match !i mod 4 with 0->Printf.printf "-" |1->Printf.printf "\\" | 2->Printf.printf "|" | _->Printf.printf "/" );
          l1:=(!i)::(!l1);
          l2:=(!mistake)::(!l2);
          Printf.fprintf out_channel_disque "%010d %9f\n" !i !mistake;
          (*if abs (!mistake -. !last_erreur) <0.0000001 then anti_poids_nul res;*)
          (*go_on := abs (!mistake -. !last_erreur) >0.00001;(* si plus de modif arrete toi*)*)
          incr i;
          last_erreur := super_erreur res tab_couples;
          flush stdout;
    done;
    Printf.printf "au bout de: %d\n" (!i*3000);
    (!l1,!l2)
  with Sys.Break -> Printf.printf "au bout de: %d\n" (!i*3000);
    (!l1,!l2)
;;


let super_train_log_eta (res:reseau) tab_couples eta nb_test_max sigmoide=(*pseudo logarithmique, sert juste a mieux visualiser la pente initiale*)
let pas=ref eta in
let mistake=ref 1. in
let last_erreur=ref 1. in
let i=ref 0 in
let go_on=ref true in
let l1,l2=ref [],ref [] in
let saut=10000 in
Sys.catch_break true;
try
while !go_on && (*!i<= 1000000 &&*)  !last_erreur > 0.001(*ceci est la borne sup des erreurs accept�es*) && !i < nb_test_max do
	Printf.printf "%f\n" (!mistake);
	entrainement res tab_couples !pas (saut) sigmoide;
	mistake := super_erreur res tab_couples;
	(match !i mod 4 with 0->Printf.printf "-" |1->Printf.printf "\\" | 2->Printf.printf "|" | _->Printf.printf "/" );
	l1:=(!i)::(!l1);
	l2:=( !mistake)::(!l2);
	(*if abs (!mistake -. !last_erreur) <0.0000001 then anti_poids_nul res;*)
	(*go_on := abs (!mistake -. !last_erreur) >0.00001;(* si plus de modif arrete toi*)*)
	if abs (!mistake -. !last_erreur) < 0.0001 then pas := !pas *. 0.99 else pas := 1.01*. !pas;
	Printf.printf "le pas est %f\n" !pas;
	incr i;
	last_erreur := super_erreur res tab_couples;
	flush stdout;
done;
	Printf.printf "au bout de: %d\n" (!i*saut);
	if !i=nb_test_max then Printf.printf "raison d'arret : nb_test_max (%d) atteint\n" (nb_test_max*saut)  else Printf.printf "raison d'arret: crit�re de boucle rempli\n";
	(!l1,!l2)
with Sys.Break -> Printf.printf "au bout de: %d\n" (!i*saut);
(!l1,!l2)
;;


let save_struct reseau file=
  let out_channel=open_out file in
  let _=Marshal.to_channel out_channel reseau [] in
  close_out out_channel

let load_struct file=
  let in_channel=open_in file in
  let reseau=Marshal.from_channel in_channel in
  let _=close_in in_channel in
  reseau

(* sauvegarde les poids avec une pr�cision de 20 chiffres *)
let save_struct_printf_scanf (reseau:reseau) file=
  let fichier=open_out file in
  Printf.fprintf fichier (*taille du reseau*) "%d\n" (Array.length reseau);
    for couche=0 to (Array.length reseau) - 1 do
      Printf.fprintf fichier (*taille de la couche*) "%d\n" (Array.length reseau.(couche));
      for neur=0 to Array.length reseau.(couche) -1 do
        Printf.fprintf fichier (*taille des entrees des neurones*) "%d\n" (Array.length reseau.(couche).(neur).poids) ;
        for weight=0 to Array.length reseau.(couche).(neur).poids - 1 do
          Printf.fprintf fichier "%.20f\n" reseau.(couche).(neur).poids.(weight)
        done
      done
    done;
  Printf.fprintf fichier "bien termine";
close_out fichier
;;



let load_struct_printf_scanf file=
	let in_channel=open_in file in
	let read_int=function x -> int_of_string (input_line x) in
	let read_float=function x -> float_of_string (input_line x) in
	let nb_couche=read_int in_channel in
	let new_neur taille nbre= 
		begin 
			let tmp=Array.make taille 0. in
			{poids=tmp ;activation=0. ;sortie=0. ; sensib= 0.}
		end in
	let reseau=Array.make nb_couche [||] in
	for couche=0 to nb_couche -1 do
		reseau.(couche) <- 
			begin 
				let taille_couche=read_int in_channel in
				let tmp = Array.make taille_couche (new_neur 0 1.) in
				for neur=0 to taille_couche-1 do
					let taille_prec=read_int in_channel in
					tmp.(neur) <- new_neur (taille_prec) 1.;
					for weight=0 to taille_prec-1 do
						tmp.(neur).poids.(weight)<-read_float in_channel
					done
				done;
				tmp
			end;
	done;
	close_in in_channel;
	Printf.printf "bien importe\n";
	(reseau:reseau)
;;