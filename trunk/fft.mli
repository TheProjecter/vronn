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
val moyenne_des_canaux : int -> int -> in_channel -> float

(** À partir d'un nom de fichier, renvoie une queue des valeurs contenues dans
   un fichier wav ainsi que deux entiers qui ne servent que pour la
   programmation *)
val création_des_echantillons :
  string ->
  int * int *
  (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Bigarray.Array1.t
  Queue.t

(** Renvoie les coefficients de la transformée de fourier pour un fichier wav *)
val transformée_de_fourier : string -> float array Queue.t

(** main peut-être utilisée à la place de transformée_de_fourier *)
val main : string -> float array Queue.t
