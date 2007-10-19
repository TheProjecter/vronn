exception Pas_bonne_taille_mult;;
exception Pas_bonne_taille_plus;;
exception Pas_bonne_taille_moins;;
exception Erreur;;
exception Pas_carree;;
exception Pb_dans_la_def;;
open Printf;;
(*   syntaxe : printf "Erreur: %s\n" s    *)

(*-------------operations de base-----------------*)
let multiply matrix1 matrix2 =
  let i1,i2,j1,j2=Array.length matrix1,Array.length matrix2,Array.length matrix1.(0),Array.length matrix2.(0) in
  let res=Array.make_matrix i1 j2 matrix1.(0).(0) in
  let somme=ref matrix1.(0).(0) in
  if i2 <> j1 
	then raise Pas_bonne_taille_mult
	else for i=0 to i1-1 do
			for j=0 to j2-1 do
				somme:= 0.;
				for k=0 to i2-1 do
				  somme := !somme +. matrix1.(i).(k) *. matrix2.(k).(j)
				done;
			res.(i).(j) <- !somme
	    done 
    done;
	res
;;
				
let (--) matrix1 matrix2=
  let i1,i2,j1,j2=Array.length matrix1,Array.length matrix2,Array.length matrix1.(0),Array.length matrix2.(0) in
  let res=Array.make_matrix i1 j1 matrix1.(0).(0) in
  if i1 <> i2 || j1 <> j2
	then raise Pas_bonne_taille_moins
	else for i=0 to i1-1 do
			for j=0 to j1-1 do
				res.(i).(j) <-matrix1.(i).(j) -.matrix2.(i).(j)
				done done;
				res
				;;
				
let (++) matrix1 matrix2=
  let i1,i2,j1,j2=Array.length matrix1,Array.length matrix2,Array.length matrix1.(0),Array.length matrix2.(0) in
  let res=Array.make_matrix i1 j1 matrix1.(0).(0) in
  if i1 <> i2 || j1 <> j2
	then raise Pas_bonne_taille_plus
	else for i=0 to i1-1 do
			for j=0 to j1-1 do
				res.(i).(j) <-matrix1.(i).(j) +.matrix2.(i).(j)
				done done;
				res
				;;

let apply f matrix=
	let i1,j1=Array.length matrix,Array.length matrix.(0) in
	let res=Array.make_matrix i1 j1 (f matrix.(0).(0)) in
	for i=0 to i1-1 do
		for j=0 to j1-1 do
			res.(i).(j) <- f matrix.(i).(j)
			done done;
			res
				;;

let transpo matrix=
	let i1,j1=Array.length matrix,Array.length matrix.(0) in
	let res=Array.make_matrix j1 i1 (matrix.(0).(0)) in
	for i=0 to i1-1 do
		for j=0 to j1-1 do
			res.(j).(i) <- matrix.(i).(j)
			done done;
			res
				;;
	
(*----------------debut TIPE--------------------*)	

let propagation entree tab_poids f=
	let n=Array.length tab_poids in
	let tab_sorties=Array.make (n+1) entree in
	for i=1 to n do 
       tab_sorties.(i) (*a indice k*)<- apply f (multiply tab_poids.(i-1) (*W indice k*) tab_sorties.(i-1) (*a indice k-1*))
		done;
	tab_sorties
	;;


let gradfpoint f_point_matrix n_k:(float array array)=
  Printf.printf "n_k, %d; fpointmachin, %d\n\n" (Array.length n_k) (Array.length f_point_matrix);
	let n=Array.length f_point_matrix in
 	if Array.length f_point_matrix.(0) <> n 
		then raise Pas_carree
		else 
		if Array.length n_k <> n then raise Pb_dans_la_def else
		let res=Array.make_matrix n 1 0. in
			for i=0 to n-1 do res.(i).(0) <- f_point_matrix.(i).(i) n_k.(i).(0) done;
			res
;;

(*let sensib sortie_desiree tab_sorties f_point_matrix tab_poids=
	let m=Array.length tab_poids in	 
	let erreur= (--) sortie_desiree tab_sorties.(m) (*derniere sortie dun tableau de m+1 cases*) in
	let tmp= multiply (gradfpoint f_point_matrix tab_sorties.(m-1)) erreur in
	let s_M=apply (fun x-> ( -2. ) *. x ) tmp in
	let tab_sensib= Array.make m s_M in
	for i=1 to (m-1) do	
		tab_sensib.(m-1-i) <- multiply ( multiply (gradfpoint f_point_matrix tab_sorties.(m-i+1) ) (transpo tab_poids.(m-i) ) ) tab_sensib.(m-i);
		done;
	tab_sensib
	;;*)

let sensib sortie_desiree (tab_sorties:(float array array array)) f_point_matrix tab_poids=
	let m=Array.length tab_poids in	 
	let erreur= (--) sortie_desiree tab_sorties.(m) (*derniere sortie dun tableau de m+1 cases*) in
	let tmp= multiply (gradfpoint f_point_matrix.(m-1) tab_sorties.(m)) erreur in
	let s_M=apply (fun x-> (* -2. *. *) x ) tmp in
	let tab_sensib= Array.make m s_M in
	for i=1 to (m-1) do	
		tab_sensib.(m-1-i) <- multiply ( multiply (gradfpoint f_point_matrix.(m-i-1) tab_sorties.(m-i) ) (transpo tab_poids.(m-i-1) ) ) tab_sensib.(m-i);
		done;
	tab_sensib
	;;

let mAJ tab_poids tab_sensib tab_sorties eta=
	for i=0 to Array.length tab_poids -1 do
		tab_poids.(i) <- tab_poids.(i) -- (apply (fun x -> (0. -. eta) *. x) (multiply tab_sensib.(i) (transpo tab_sorties.(i))));
  done
	;;



let entrainement tab_poids tab_couple f f_point_matrix eta=
	let n=Array.length tab_couple -1 in let p1,d1=tab_couple.(0) in let p,d=ref p1,ref d1 and tab_sorties,tab_sensib=ref [||],ref [||] in
	for i=0 to n do
		p := fst tab_couple.(i);
    d := snd tab_couple.(i);
		tab_sorties := propagation !p tab_poids f;
		tab_sensib := sensib !d !tab_sorties f_point_matrix tab_poids;
		mAJ tab_poids !tab_sensib !tab_sorties eta
		done
	;;




		
(*-----------commentaires------------------*)

(*   f_point_matrix est une matrice diagonale contenant en (i,i) la fonction derivee de f par rapport à la i-eme variable (cf poly)
!!! la taille de f_point_matrix depend de la couche sur laquelle elle est utilisee car elle est appliquée au vecteur denntree de dimension le nombre de neurone de la couche precedente   donc il faudra lutlier sous forme de tableau de f_point_matrix avec les dim requises*)


let et = [|[|[|-0.30;0.43|]|]|];;
for i=0 to 10 do
entrainement 
  et  
  ([| ( [|[|0.|];[|1.|]|], [|[|0.|]|] );( [|[|1.|];[|1.|]|], [|[|1.|]|] );( [|[|0.|];[|0.|]|], [|[|0.|]|] );( [|[|1.|];[|0.|]|], [|[|0.|]|] );|])
  (fun x->if x>0.5 then 1. else 0.)
  [|[|[|fun x->1.|]|]|]
  0.1
done
;;
et;;
propagation [|[|1.|];[|1.|]|] et (fun x->x);;

let xor = [|[|[|-0.6;0.7|];[|0.3;0.4|]|];[|[|0.2;0.5|]|]|];;
entrainement
  xor
  ([| ( [|[|0.|];[|1.|]|], [|[|1.|]|] );( [|[|1.|];[|1.|]|], [|[|0.|]|] );( [|[|0.|];[|0.|]|], [|[|0.|]|] );( [|[|1.|];[|0.|]|], [|[|1.|]|] );|])
  (fun x->if x>0.5 then 1. else 0.)
  [|[|[|(fun x->1.);(fun x->0.)|];[|(fun x->0.);(fun x->1.)|]|];[|[|(fun x->1.)|]|]|]
  0.3
;;

#trace propagation;;
#trace multiply;;
#trace sensib;;
#trace gradfpoint;;
