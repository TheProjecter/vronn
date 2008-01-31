(*let input_string=BinaryParse.input_string
let gen_fct=BinaryParse.gen_fct
type value=BinaryParse.value*)
open BinaryParse
exception Unknown_type;;

(* une petite définition de fainéant, heu... qui améliore la lisibilité du code *)
let read_ui8n=little_endian_read_ui8n
let int_of_I=function I n->n |_-> assert false 

(* définition de la liste de parsing *)
let rec parsing_list_of_chunk_id hashtbl=
  match Hashtbl.find hashtbl "next_chunk_id" with
    |S "fmt "->wav_format_chunk
    |S "data"->wav_data_chunk
    |_->assert false
and type_of_riff_chunk hashtbl=
  match Hashtbl.find hashtbl "type" with
    |S "WAVE"->[input_string 4, "next_chunk_id", Some (parsing_list_of_chunk_id)]
    |_->raise Unknown_type
and wav_data_chunk=[
  read_ui8n 4, "data_length", None
]
and wav_format_chunk=[
  read_ui8n 4, "format_chunk_length", None;
  read_ui8n 2, "compression_code", None;
  read_ui8n 2, "number_of_channels", None;
  read_ui8n 4, "sample_rate", None;
  read_ui8n 4, "average_bps", None;
  read_ui8n 2, "block_align", None;
  read_ui8n 2, "significant_bits_per_sample", None;
  input_string 4, "next_chunk_id", Some (parsing_list_of_chunk_id)
]
and riff_chunk=[
  input_string 4, "RIFF", None;
  read_ui8n 4, "total_length", None;
  input_string 4, "type", Some (type_of_riff_chunk);
]

let moyenne_des_canaux alignement nombre_de_canaux in_channel=
  let placeholder=ref 0. in
  for i=0 to nombre_de_canaux-1 do
    placeholder:=!placeholder+.float (int_of_I (read_ui8n (alignement/nombre_de_canaux) in_channel)/nombre_de_canaux)
  done;
  !placeholder

let creation_des_echantillons fichier=
  let in_channel,parsed_header=header_parse (open_in_bin fichier) riff_chunk in 
  let valeur nom=int_of_I (Hashtbl.find parsed_header nom) in 

  (* récupération des valeurs stockées dans la table de hachage et 
  * calcul de certaines autres à partir de celles-ci *)
  let dt=10 in (* millisecondes *)
  let echantillonage=(valeur "sample_rate") in 
	let alignement=valeur "block_align" in
  let nombre_de_canaux=(valeur "number_of_channels") in 
  let echantillons_par_dt=echantillonage*dt/1000 in 
  let bits_par_dt=(valeur "significant_bits_per_sample")*echantillons_par_dt in 
  let paquets_d'echantillons=(valeur "data_length")*8/(bits_par_dt*nombre_de_canaux) in
  (* debug: affiche toutes les valeurs stockées dans la table de hachage *)
  (*let prnt a=function
    |S s -> Printf.printf "%s : %s" a s; print_endline ""
    |I i -> Printf.printf "%s : %d" a i; print_endline ""
  in
  let _=Hashtbl.iter prnt parsed_header in
  let _=print_endline (string_of_int paquets_d'echantillons) in
  *)
  let queue_des_echantillons=Queue.create () in 
  let bigarray_courante=Bigarray.Array1.create Bigarray.complex64 Bigarray.c_layout echantillons_par_dt in
  for i=0 to paquets_d'echantillons-1 do
    for j=0 to echantillons_par_dt-1 do
      bigarray_courante.{j} <- {Complex.re=(moyenne_des_canaux alignement nombre_de_canaux in_channel); Complex.im=0.}
    done;
    Queue.add bigarray_courante queue_des_echantillons 
  done;
  echantillons_par_dt,queue_des_echantillons

let queue_map f q=
  let q_prime=Queue.create () in
  for i=0 to Queue.length q -1 do
    Queue.add (f (Queue.take q)) q_prime;
  done;
  q_prime

let bigarray1_map f b=
  for i=0 to Bigarray.Array1.dim b -1 do
    b.{i} <- f b.{i};
  done;
  b

let cepstre fichier=
  let echantillons_par_dt,queue_des_echantillons=creation_des_echantillons fichier in
  let spectres=queue_map (Fftw2.create Fftw2.forward echantillons_par_dt) queue_des_echantillons in
(*  let spectre_en_amplitude=queue_map (bigarray1_map (function z -> z.Complex.re)) spectres in*)
  let phase=queue_map (bigarray1_map Complex.log) spectres in
  queue_map (Fftw2.create Fftw2.backward echantillons_par_dt) phase

let spectre fichier=
  let echantillons_par_dt,queue_des_echantillons=creation_des_echantillons fichier in
  queue_map (Fftw2.create Fftw2.forward echantillons_par_dt) queue_des_echantillons

let re_array_of_cplx_bigarray1_norm norm bigarray=
  Array.init (Bigarray.Array1.dim bigarray) (function i->bigarray.{i}.Complex.re /. norm)
