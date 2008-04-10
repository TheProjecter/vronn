open Reseaux

let seuil = 0.1

let queue = 
	let t = Fft.queue_map (Fft.array_of_res_norm_moy 8800000. 3) (Fft.spectre ("./whee.wav"))
	in Fft.queue_map (fun tab -> Array.sub tab 0 500) t

let lgr = Queue.length queue

let res=load_struct "./results/notes_struct_all_200"

let n = Array.length res.(Array.length res -1)

let notesreconnues = Queue.create ()


let gammes = (* [|"3"; "4"|] *)[|"3";"4";"5";"6"|]
let notes = (*[|"c";"e";"g";"a"|]*)[|"c";"d";"e";"f";"g";"a";"b"|]
let nb_notes = Array.length notes
let nb_simples = nb_notes * Array.length gammes
let nb_couples = (* nb_simples * 2 - 3*) nb_simples * (nb_simples - 1) / 2
let simples = Array.init nb_simples (fun i -> gammes.(i / nb_notes) ^ notes.(i mod nb_notes))
let couples =
  let t= Array.make nb_couples "" and c = ref 0 in
  (try
	for i = 0 to nb_simples-1 do
    for j = i+1 to nb_simples-1 do
      t.(!c) <- simples.(i) ^ "," ^ simples.(j); incr c; if !c >= nb_couples then failwith "fin";
    done;
  done;
  with Failure "fin" -> ());
  t


let nb_files = nb_simples + nb_couples
let files = Array.concat [simples; couples]

let _=
	for i= 0 to lgr-1 do
		let t=propa (Queue.pop queue) res and lst = ref [] in
		for j = 0 to n-1 do
			if t.(j) > seuil then lst:=(files.(j),t.(j))::(!lst)
		done;
		Queue.push !lst notesreconnues
	done;

Queue.iter (fun x-> List.iter (fun (s,i) -> Printf.printf "%s : %f, " s i) (List.rev x); print_newline()) notesreconnues
