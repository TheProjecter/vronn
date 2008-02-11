open Reseaux

let queue = Fft.queue_map (Fft.array_of_res_norm_moy 880000. 100) (Fft.spectre ("./court.wav"))
let lgr = Queue.length queue

let res=load_struct "./results/notes_struct"

let n = Array.length res.(Array.length res -1)

let notes = Queue.create ()

let files = Sys.readdir "./notes/"

let _=
	let ordre str1 str2 = match compare (String.length str1) (String.length str2) with
  |0 -> compare str1 str2
  |i -> i
	in Array.sort ordre files;
	for i= 0 to lgr-1 do
		let t=propa (Queue.pop queue) res and lst = ref [] in
		for j = 0 to n-1 do
			if t.(j) > 0.6 then lst:=files.(j+1)::(!lst)
		done;
		Queue.push !lst notes
	done;

Queue.iter (fun x -> List.iter (Printf.printf "%s, ") x; print_newline()) notes







