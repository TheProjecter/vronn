(*let input_string=BinaryFileParse.input_string
let gen_fct=BinaryFileParse.gen_fct
type value=BinaryFileParse.value*)
open BinaryFileParse
exception Unknown_type;;

(* une petite définition de fainéant, heu... qui améliore la lisibilité du code *)
let read_ui8n=little_endian_read_ui8n
let int_of_I=function I n->n 
let moyenne_des_canaux profondeur nombre_de_canaux in_channel=
  let placeholder=ref 0. in
  let _=for i=0 to nombre_de_canaux-1 do
    placeholder:=!placeholder+.float ((int_of_I (read_ui8n profondeur in_channel))/nombre_de_canaux)
  done in
  !placeholder

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

let création_des_échantillons fichier=
  let in_channel,header_parsé=BinaryFileParse.header_parse (open_in_bin fichier) riff_chunk in 
  let valeur nom=int_of_I (Hashtbl.find header_parsé nom) in 

  (* récupération des valeurs stockées dans la table de hachage et 
  * calcul de certaines autres à partir de celles-ci *)
  let dt=10 in (* millisecondes *)
  let échantillonage=(valeur "sample_rate") in 
  let profondeur=(valeur "significant_bits_per_sample")/8 in 
  let nombre_de_canaux=(valeur "number_of_channels") in 
  let échantillons_par_dt=échantillonage*dt/1000 in 
  let bits_par_dt=(valeur "significant_bits_per_sample")*échantillons_par_dt in 
  let paquets_d'échantillons=(valeur "data_length")/bits_par_dt in 
  (* debug: affiche toutes les valeurs stockées dans la table de hachage
    * let prnt a=function
    |S s -> Printf.printf "%s : %s" a s; print_endline ""
    |I i -> Printf.printf "%s : %d" a i; print_endline ""
  let _=Hashtbl.iter prnt header_parsé
  let _=print_endline (string_of_int paquets_d'échantillons)
  *)

  let queue_des_échantillons=Queue.create () in 
  let bigarray_courante=ref (Bigarray.Array1.create Bigarray.complex64 Bigarray.c_layout 0) in
  let _=for i=0 to paquets_d'échantillons-1 do
    let _=bigarray_courante:=Bigarray.Array1.create Bigarray.complex64 Bigarray.c_layout échantillons_par_dt in
    let _=for i=0 to échantillons_par_dt-1 do
      Bigarray.Array1.set !bigarray_courante i {Complex.re=(moyenne_des_canaux profondeur nombre_de_canaux in_channel); Complex.im=0.}
    done in
    Queue.add !bigarray_courante queue_des_échantillons 
  done in
  échantillons_par_dt,nombre_de_canaux,paquets_d'échantillons,queue_des_échantillons

let transformée_de_fourier fichier=
  let échantillons_par_dt,nombre_de_canaux,paquets_d'échantillons,queue_des_échantillons=création_des_échantillons fichier in
  let fft=Fftw2.create Fftw2.backward échantillons_par_dt in
  let queue_des_coefficients=Queue.create () in 
  let _=for i=0 to paquets_d'échantillons-1 do
    Queue.add (fft (Queue.take queue_des_échantillons)) queue_des_coefficients 
  done in
  queue_des_coefficients

let main=transformée_de_fourier;;

