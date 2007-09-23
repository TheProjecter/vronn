type poids = float array 
and neurone = poids*(poids->float array-> float*float) 
and couche = neurone array 
and reseau = couche list;;

exception Malappel of string;;
exception Pasbonnetaille of string

let sigmoide (poids:poids) entrees= let somme = ref 0. and n=Array.length poids in
 if Array.length entrees <> n then raise (Pasbonnetaille "sigmoide");
 for i=0 to Array.length poids -1 do
  somme:=!somme +. poids.(i) *. entrees.(i)
 done;
 (1. -. 1. /. (1. +. exp !somme)),(-. exp !somme /. (( 1. +. exp !somme) *. (1. +. exp !somme))) (* sig,desig *)
;;

let runcouche (couche:couche) entrees =
  let nn = Array.length couche and ne = Array.length entrees in
  if Array.length (fst couche.(0)) <> ne then raise (Pasbonnetaille "runcouche");
  let sortie = Array.make nn 0. and dF = Array.make nn 0. in
  for i=0 to nn-1 do
   let res,df = (snd couche.(i)) (fst couche.(i)) entrees in
   sortie.(i) <- res; dF.(i)<-df
  done;
  sortie,dF
;; 

let rec testres (reseau:reseau) entrees =
 match reseau with 
  |[]   -> entrees
  |t::q -> testres q (fst (runcouche t entrees))
;;

let rec runres (reseau:reseau) entrees =
 match reseau with
  |[]   -> entrees,[],[]
  |t::q -> let sortie,dF = runcouche t entrees in
    let sortie,lst_E,lst_F = runres q sortie in
    sortie,entrees::lst_E,dF::lst_F
;;

let (--) vect1 vect2 = let n = Array.length vect1 in
 if Array.length vect2 <> n then raise (Pasbonnetaille "distance");
 let res = Array.make n (vect1.(0) -. vect2.(0)) in
 for i=1 to n-1 do
  res.(i) <- vect1.(i) -. vect2.(i)
 done;
 res
;;

let sensibdebase dF sortie_reelle sortie_desiree =
  let n = Array.length sortie_reelle and diffsortie = sortie_reelle -- sortie_desiree in 
  let res = Array.make n 0. in
  for i = 0 to n-1 do
   res.(i) <- (* -. 2. *. *) dF.(i) *. diffsortie.(i)
  done;
 res
;;

let rec sensib (reseau:reseau) lst_F sortie_reelle sortie_desiree =
 match reseau,lst_F with
  |_,[] -> raise (Malappel "sensib") 
  |[],[dF] -> (sensibdebase dF sortie_reelle sortie_desiree)::[]
  |couche::qres,dF::qF -> let nn = Array.length couche and ne = Array.length (fst couche.(0)) in 
   if Array.length dF <> ne then raise (Pasbonnetaille "sensib"); 
   let tmp = Array.make_matrix nn ne 0. in
    for i=0 to nn-1 do
     for j=0 to ne-1 do
      tmp.(i).(j) <- (* -. 2. *. *) (fst couche.(i)).(j) *. dF.(j)
     done;
    done;
    let skplusun::reste = sensib qres qF sortie_reelle sortie_desiree in
     let resultat = Array.make ne 0. in
     if Array.length skplusun <> nn then raise (Pasbonnetaille "sensib skplusun");
     for i=0 to ne-1 do
      for j=0 to nn-1 do
       resultat.(i)<-resultat.(i) +. tmp.(j).(i) *. skplusun.(j)
      done;
     done;
    resultat::skplusun::reste
;;

let rec majpoids eta (reseau:reseau) lst_E lst_S = 
 match reseau,lst_E,lst_S with
  |[],[],[]->()
  |[],_,_ |_,[],_ |_,_,[] -> raise (Malappel "majpoids")
  |couche::qres,e::qE,s::qS -> majpoids eta qres qE qS;
    for i=0 to Array.length couche - 1 do
     for j=0 to Array.length (fst couche.(0)) -1 do
       (fst couche.(i)).(j) <- (fst couche.(i)).(j) -. eta *. s.(i) *. e.(j)
     done;
    done
;;

let trainres (reseau:reseau) entrees sortie_desiree eta =
 let sortie_reelle,lst_E,lst_F=runres reseau entrees in
 let lst_S = sensib (List.tl reseau) (*[list.hd reseau]*) lst_F sortie_reelle sortie_desiree in
 majpoids eta reseau lst_E lst_S;
;;

let training (reseau:reseau) tab_ES etaa precision =
 let eta = ref etaa and n=Array.length tab_ES in
 while !eta > precision do
  let k=Random.int n in
  trainres reseau (fst tab_ES.(k)) (snd tab_ES.(k)) !eta;
  eta:=!eta *. 0.99995;
 done
;;

let rec randW lst_n n_e fonction =
 match lst_n with
  |[] -> [] 
  |t::q->let finit _ = let pattern = Array.make n_e 0. in
   for i = 0 to n_e - 1 do
     pattern.(i) <- float_of_int ( Random.int 20 - 10 ) /. 100.
   done;
   pattern,fonction
  in (Array.init t finit)::(randW q t fonction)
;;


(*
(* -------------------------------------------------------------------------- *)
(* --------- We assume here that the training works, which doesn't. --------- *)
(* -------------------------------------------------------------------------- *)

let lire file i =
  (* open file, set current to i*10ms_en_char, renvoyer une string de 10ms *)
  ""
;;

let taille file =
 (* open file, regarder le nombre de 10ms qui tiennent, et renvoyer ça *)
 0
;;

let mange food =
 (* fais passer dans le réseau en soit *)
 [|0;0;0|]
;;

let analyze résultat =
 (* récupère le tableau des résultats, et essaye d'en faire quelque chose. *)
 ()
;;

let reseau_son file =
  let n = taille file in let resultats = Array.make n [|0;0;0|] in
  for i=0 to n-1 do
    let food = lire file i in
    let plusprobable = mange food in resultats.(i) <- plusprobable
  done;
  analyze resultats
;;
*)
