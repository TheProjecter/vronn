let funlog (poids:poids) entrees = 
let somme = ref 0. and n=Array.length poids in
 if Array.length entrees <> n then raise (Pasbonnetaille "sigmoide");
 for i=0 to Array.length poids -1 do
  somme:=!somme +. poids.(i) *. entrees.(i)
 done;
 if !somme > 0.5 then 1.,1.
                 else 0.,1.
;;

let ident (poids:poids) entrees =
	let somme = ref 0. and n=Array.length poids in
 if Array.length entrees <> n then raise (Pasbonnetaille "sigmoide");
 for i=0 to Array.length poids -1 do
  somme:=!somme +. poids.(i) *. entrees.(i)
 done;
 !somme,1.	
;;


(* let (et:couche) = [|[|0.45;0.45|],funlog|];;
runcouche et [|0.;1.|];;

let (ou:couche) = [|[|0.55;0.55|],funlog|];;
runcouche ou [|1.;0.|];;

let (xor:reseau) = [[|[|0.45;0.45|],funlog;[|0.55;0.55|],funlog|];[|[|-1.;1.|],funlog|]];;
testres xor [|0.;1.|];;*)

let et = randW [1] 2 funlog;;
training et [|[|1.;1.|],[|1.|];[|1.;0.|],[|0.|];[|0.;1.|],[|0.|];[|0.;0.|],[|0.|]|] 1. 0.001;;
testres et [|1.;1.|];;
et;;

let xor = randW [2;1] 2 funlog;;
for i=0 to 4 do
training xor [|[|1.;1.|],[|0.|];[|1.;0.|],[|1.|];[|0.;1.|],[|1.|];[|0.;0.|],[|0.|]|] 5. 0.005
done;;
Printf.printf "1,1 : %f\n1,0 : %f\n0,1 : %f\n\n" (testres xor [|1.;1.|]).(0) (testres xor [|1.;0.|]).(0) (testres xor [|0.;1.|]).(0);;
xor;;

let compare = randW [1] 2 funlog;;
let mat=Array.make 100 ([|0.;0.|],[|0.|]);;
for i=0 to 99 do
  let j=float_of_int (Random.int 500) and k=float_of_int (Random.int 500) in
   if j<=k then mat.(i)<-[|j;k|],[|1.|] else mat.(i)<-[|j;k|],[|0.|]
done;;

training compare mat 0.5 0.005;;
testres compare [|501.;500.|];;
compare;;

let plus = randW [1] 2 ident;;
let mat2=Array.make 100 ([|0.;0.|],[|0.|]);;
for i=0 to 99 do
  let j=float_of_int (Random.int 500) and k=float_of_int (Random.int 500) in
    mat2.(i)<-[|j;k|],[|j+.k|]
done;;
training plus mat2 0.5 0.005;;
testres plus [|63.;84.|];;
plus;;
