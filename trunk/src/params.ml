let reload_res = false
let progressive_training = true
let dt = 50
let moy = 1
let norme = 8800000.
let debut_tab_fft= 0
let taille_tab_fft= 80
let div_sample_per_file = 2


let gammes = (* [|"3";"4"|] *) [|"3";"4";"5";"6"|]
let notes = (*[|"c";"e";"g";"a"|]*)  [|"c";"d";"e";"f";"g";"a";"b"|]
let nb_notes = Array.length notes
let nb_simples = nb_notes * Array.length gammes
let nb_couples = (*nb_simples * 2 - 3 *) nb_simples * (nb_simples - 1) / 2 
let simples = Array.init nb_simples (fun i -> gammes.(i / nb_notes) ^ notes.(i mod nb_notes))
let couples = 
  let t= Array.make nb_couples "" and c = ref 0 in
  (try 
  for i = 0 to nb_simples-1 do
    for j = i+1 to nb_simples-1 do
      t.(!c) <- simples.(i) ^ "," ^ simples.(j); incr c; if !c >= nb_couples then failwith "fin"
    done;
  done;
  with Failure "fin" -> ());
  t

let nb_files = nb_simples + nb_couples
let files = Array.concat [simples; couples]

let taille_res = [|80;40;nb_simples|]
