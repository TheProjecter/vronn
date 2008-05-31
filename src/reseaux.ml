Gc.set {(Gc.get()) with Gc.minor_heap_size=200000};

Random.init (int_of_float (Unix.time ()));

Sys.catch_break true;

type neurone=
  {poids: float array; mutable activation:float; mutable sortie:float;mutable sensib: float}
and reseau=neurone array array

let t=ref 0.
let sigmoide =fun x->1./. (1. +.exp (-.x))
let dsigm x=
  let exp_moins_x=exp (-.x) in
  exp_moins_x /. (1. +. exp_moins_x) ** 2.
let absf x=
  if x>0. then x else -.x;;


let propagation entree (res:reseau)=
  let m=Array.length res in
  for neur=0 to Array.length res.(0) -1 do
    t:=0.;
    for i=0 to Array.length entree -1 do
      t:=!t +. (res.(0).(neur).poids.(i) *. entree.(i));
    done;
    res.(0).(neur).activation <- !t -. res.(0).(neur).poids.(Array.length entree);
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
        res.(couche).(neur).poids.(Array.length res.(couche_moins_un));
      res.(couche).(neur).sortie <- sigmoide res.(couche).(neur).activation
    done
  done
;;

let propa entrees reseau =
	propagation entrees reseau;
	let lastlayer = reseau.(Array.length reseau -1) in
	let n= Array.length lastlayer in
	let res = Array.make n 0. in
	for i=0 to n-1 do
		res.(i) <- lastlayer.(i).sortie
	done;
	res
;;

(*
let propagation entree reseau =
  let somme_pond entree coeffs =
    Array.fold_left (+.) 0.
      (Array.mapi (fun i v -> v.sortie *. coeffs.(i)) entree) in
 
  let gere_couche couche entree =
    let rajout_biais i neur =
      let activation = somme_pond entree neur.poids -. neur.poids.(Array.length entree) in
      neur.activation <- activation;
      neur.sortie <- sigmoide activation in
    Array.iteri rajout_biais couche in
 
  Array.iteri
    (fun i couche -> gere_couche couche
       (if i = 0 then Array.map (fun x -> {sortie=x; poids=[||]; activation = 0.; sensib=0.}) entree else reseau.(i-1)))
    reseau
*)

let calcul_sensib (res:reseau) entree sortie eta=
  (*calcul de base*)
  let m=Array.length res in
	let lastlayer = res.(m-1) in
  for neur=0 to Array.length res.(m-1) -1 do 
    res.(m-1).(neur).sensib <- (dsigm lastlayer.(neur).activation) *. (sortie.(neur) -. lastlayer.(neur).sortie)
  done;
  (*calcul recursif*)
  let somme=ref 0. in
  for couche=m-2 downto 0 do
    for neur=0 to Array.length res.(couche) -1 do
      somme:=0.;
      let previouslayer= res.(couche+1) in
      for k=0 to Array.length previouslayer -1 do
        somme := !somme +. (previouslayer.(k).sensib *. previouslayer.(k).poids.(neur))
      done;
      res.(couche).(neur).sensib <- (dsigm res.(couche).(neur).activation) *. !somme;
      for k=0 to Array.length previouslayer -1 do
				let poids = previouslayer.(k).poids in
        let t=eta *. previouslayer.(k).sensib in
        poids.(neur) <- poids.(neur) +. (t *. res.(couche).(neur).sortie);
        poids.(Array.length res.(couche)) <-
          poids.(Array.length res.(couche)) -. t;
      done;
    done;
  done;
  for input=0 to Array.length entree-1 do
		let firstlayer = res.(0) in
    for k=0 to Array.length firstlayer -1 do
      let t=eta *. firstlayer.(k).sensib in
      firstlayer.(k).poids.(input) <- firstlayer.(k).poids.(input) +. (t *. entree.(input) );
      firstlayer.(k).poids.(Array.length entree) <-firstlayer.(k).poids.(Array.length entree) -. t;
    done;
  done
;;


let generation (nb_neurone_par_couche:int array) taille_entree=
  let nb_couche=Array.length nb_neurone_par_couche in
  let new_neur taille= 
    let tmp=Array.make taille 0. in
    for i=0 to taille -1 do
      tmp.(i) <- Random.float 1. -. 0.5 
    done;
    {poids=tmp ;activation=0. ;sortie=0. ; sensib= 0.}
  in
  let reseau=Array.make nb_couche [||] in
  let tmp = Array.make nb_neurone_par_couche.(0)   (new_neur 0) in
  for j=0 to nb_neurone_par_couche.(0)- 1 do
    tmp.(j) <- new_neur (taille_entree+1) (*+1: a cause du biais*)
  done;
  reseau.(0) <- tmp;
  for couche=1 to nb_couche -1 do
    let tmp = Array.make nb_neurone_par_couche.(couche) (new_neur 0) in
		let nb_neurone_sur_couche_moins_un=nb_neurone_par_couche.(couche-1) in
    for j=0 to nb_neurone_par_couche.(couche)-1 do
      tmp.(j) <- new_neur (nb_neurone_sur_couche_moins_un +1) (*+1: a cause du biais*)
    done;
    reseau.(couche) <- tmp;
  done;
  (reseau:reseau)
;;

let entrainement (reseau:reseau) tab_couples eta nb_test_max=
	let n=Array.length tab_couples  in 
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	for i=0 to nb_test_max do
		p := fst tab_couples.(i mod n);
		d := snd tab_couples.(i mod n);
		propagation !p reseau;
		calcul_sensib reseau !p !d eta;
	done
;;

let super_erreur res tab_couples (* valable pour tout type de reseau*)=
	let p1,d1=tab_couples.(0) in 
	let p,d=ref p1,ref d1 in
	let n=Array.length tab_couples in
        let long_res=Array.length res in
	let erreur =ref 0. in
	let erreur2=ref 0. in
	for i=0 to  n -1 do
		erreur:=0.;
		p := fst tab_couples.(i);
		d := snd tab_couples.(i);
		propagation !p res;
		for j=0 to Array.length (res.(long_res -1)) -1 do
			let tmp= absf (!d.(j)-. res.(long_res -1).(j).sortie)  in
			erreur := !erreur +. (tmp *. tmp);
		done;
		erreur2:=!erreur2 +. sqrt (!erreur);
	done;
	!erreur2 /. (float_of_int n);
;;

let super_train_log_eta (res:reseau) tab_couples eta nb_test_max precision =
  let pas=ref eta in
  let mistake=ref (super_erreur res tab_couples) in
  let last_erreur=ref !mistake in
	let i=ref 0 in
  let l1,l2=ref [],ref [] in
  let saut=4000 in
  let log_c=open_out_bin "./results/pour_gnuplot.txt" in
  try
    while !last_erreur > precision (*ceci est la borne sup des erreurs acceptées*) && !i < nb_test_max do
      Printf.printf "%.16f\n" (!mistake);
			(try while true do
					let changepas,_,_=Unix.select [Unix.stdin] [] [] 0. in
					match changepas with 
					| something::_ -> let buf=String.make 1 ' ' in (ignore (Unix.read something buf 0 1); match buf.[0] with | '+' -> pas := 2. *. !pas | '-' -> pas := 0.5 *. !pas | _ -> ())
					|[] -> (*let abs_diff_err = max (!mistake -. !last_erreur) (!last_erreur -. !mistake) in
						pas:=max (min (!pas *. (1. -. 0.001 *. (0.01 /. abs_diff_err-. 100. *. abs_diff_err))) 1.) 0.0000001;*)
						failwith "test";
			done; with Failure "test"-> ());
      Printf.printf "Le pas est %f\n" !pas;
			last_erreur := !mistake;
      entrainement res tab_couples !pas (saut);
			mistake := super_erreur res tab_couples;
      Printf.fprintf log_c "%d %f\n" !i !mistake;
      l1:=(!i)::(!l1);
      l2:=(!mistake)::(!l2);
      incr i;
      flush stdout;
    done;
    Printf.printf "%.16f\n" (!last_erreur); Printf.printf "Le pas est %f\n" !pas;
    Printf.printf "\nAu bout de: %d\n" (!i*saut);
    if !i=nb_test_max
      then Printf.printf "Raison d'arrêt : nb_test_max (%d) atteint\n" (nb_test_max*saut)
      else Printf.printf "Raison d'arrêt : critère de boucle rempli\n";
    (!l1,!l2)
  with Sys.Break -> Printf.printf "\nAu bout de : %d\n" (!i*saut);
    (!l1,!l2)
;;

(* sauvegarde et lit les poids avec une précision de 20 chiffres *)
let save_struct (reseau:reseau) file=
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


let load_struct file=
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

let sortie reseau =
	let n= Array.length reseau -1 in
	let sortie = Array.make (Array.length reseau.(n)) 0. in
	for i=0 to Array.length reseau.(n) -1 do
    sortie.(i) <- reseau.(n).(i).sortie
	done;
	sortie
;;		

(*
type hyper_reseau = {nom: string; reseau: reseau; test: (float array -> int) ;suite: hyper_reseau array};;

let rec hpropa {nom=nom; reseau=reseau; test=test; suite=suite} entrees noms=
	propagation entrees reseau;
	match suite with
	| [||] -> (nom::noms)
	| _ -> hpropa suite.(test (sortie reseau)) entrees (nom::noms)
;;

let rec hgen (nom,taille,test,suite) taille_entree =
	let suite_res = Array.make (Array.length suite) {nom=""; reseau=[||]; test=(fun _-> 0); suite=[||]} in
	for i=0 to Array.length suite -1 do
		suite_res.(i) <- hgen suite.(i) taille_entree
	done;
	{nom=nom; reseau=(generation taille taille_entree); test=test; suite=suite_res}
;;

*)
