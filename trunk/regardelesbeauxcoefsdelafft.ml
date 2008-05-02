open Reseaux
open Affichage
open Params

let nbr= 1

(*let files = Sys.readdir "notes";;

let ordre str1 str2 = match compare (String.length str1) (String.length str2) with
	|0 -> compare str1 str2
	|i -> i
in
Array.sort ordre files;;*)

let queues = Array.make nbr (Queue.create ());;

for i=0 to nbr-1 do 
	queues.(i) <- Fft.queue_map (fun tab -> Array.sub (Fft.array_of_res_norm_moy norme moy tab) debut_tab_fft taille_tab_fft) (Fft.spectre ("./whee.wav") dt);
done;;

(* print_endline ("Done with the Fastest Fourier Transform in the West ! (1)" );; *)


(*Queue.iter
	(fun tab-> 
							Array.iter (fun x -> Printf.printf "%10f; " x) tab;
							print_endline "----------------------------------"; print_endline "*************************") 
	queue *)


for i= 0 to 0 do
(*print_endline files.(i+1);*)
	let count = ref 1 in
	Queue.iter (fun tab -> let outchan=open_out ("gnuplot/" ^ (string_of_int !count)) in Array.iteri (fun j x -> Printf.fprintf outchan"%i %10f\n" j x) tab; close_out outchan; incr count) queues.(i);
	print_newline ();
done
