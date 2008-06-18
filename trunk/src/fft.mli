(** Génère un tableau des coefficients de la transformée de fourier à partir du
   chemin vers un fichier wav *)

(** Exception soulevée lorsque le programme n'est pas capable de s'occuper du
   fichier fournit. Peut arriver lorsque le fichier n'est pas un fichier wav
   avec un encodage pcm ou bien s'il utilise des champs du format wav que nous
   n'avons pas implémenter (visiblement à raison vu que nous n'avons pas encore
   eu cette exception). *)
exception Unknown_type

(** Le réseau de neurone n'étant pas capable de gêrer les sons multi-canaux, en
   faire la moyenne permet de ne pas perdre d'information tout en créant des
   données compréhensibles par le réseau *)
(*val moyenne_des_canaux : int -> int -> in_channel ref -> float

(** À partir d'un nom de fichier, renvoie une queue des valeurs contenues dans
   un fichier wav ainsi que deux entiers qui ne servent que pour la
   programmation *)
*)
val creation_des_echantillons :
  string -> int ->
  int * (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Bigarray.Array1.t
  Queue.t

(** Fonctions de convénience et de lisibilité *)
val queue_map : ('a -> 'b) -> 'a Queue.t -> 'b Queue.t
val bigarray1_map :
  ('a -> 'a) ->
  ('a, 'b, 'c) Bigarray.Array1.t -> ('a, 'b, 'c) Bigarray.Array1.t

(** Calcule le cepstre pour un fichier wav ; mauvais résultats pour l'instant *)
val cepstre :
  string -> int ->
  (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Bigarray.Array1.t
  Queue.t

(** Calcule le spectre pour un fichier wav ; bon résultats. Mais pas moyen de
 * savoir s'ils sont optimaux. *)
val spectre :
  string -> int ->
  (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Bigarray.Array1.t
  Queue.t

(** Transforme une Bigarray.Array1 en Array, le premier ne servant qu'à utiliser
 * fftw2 *)
val re_array_of_cplx_bigarray1_norm :
  float -> (Complex.t, 'a, 'b) Bigarray.Array1.t -> float array

(** Transforme une Bigarray.Array1 en Array, en faisant la moyenne sur n 
 * valeurs et divisant le tout par le premier argument *)
val array_of_res_norm_moy :
  float -> int -> (Complex.t, 'a, 'b) Bigarray.Array1.t -> float array
