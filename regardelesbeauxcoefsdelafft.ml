open Reseaux
open Affichage

let nbr= 1

(*let files = Sys.readdir "notes";;

let ordre str1 str2 = match compare (String.length str1) (String.length str2) with
	|0 -> compare str1 str2
	|i -> i
in
Array.sort ordre files;;*)

let queues = Array.make nbr (Queue.create ());;
let rec queuemap f queue = if Queue.is_empty queue then Queue.create () else let a = Queue.pop queue in let res = queuemap f queue in Queue.push (f a) res; res
in

for i=0 to nbr-1 do 
	queues.(i) <- Fft.queue_map (Fft.array_of_res_norm_moy 8800000. 3) (Fft.spectre ("./midge/3a.wav")) (*/^files.(i+1)))*);
	queues.(i) <- queuemap (fun tab -> Array.sub tab 0 500) queues.(i);

done;;

(* print_endline ("Done with the Fastest Fourier Transform in the West ! (1)" );; *)


(*Queue.iter
	(fun tab-> 
							Array.iter (fun x -> Printf.printf "%10f; " x) tab;
							print_endline "----------------------------------"; print_endline "*************************") 
	queue *)


for i= 0 to 0 do
(*print_endline files.(i+1);*)
	Queue.iter (Array.iteri (fun j x -> Printf.printf "%i %10f\n" j x)) queues.(i);
	print_newline ();
done
