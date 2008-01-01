(** G�n�re un tableau des coefficients de la transform�e de fourier � partir du
   chemin vers un fichier wav *)

(** Exception soulev�e lorsque le programme n'est pas capable de s'occuper du
   fichier fournit. Peut arriver lorsque le fichier n'est pas un fichier wav
   avec un encodage pcm ou bien s'il utilise des champs du format wav que nous
   n'avons pas impl�menter (visiblement � raison vu que nous n'avons pas encore
   eu cette exception). *)
exception Unknown_type

(** Le r�seau de neurone n'�tant pas capable de g�rer les sons multi-canaux, en
   faire la moyenne permet de ne pas perdre d'information tout en cr�ant des
   donn�es compr�hensibles par le r�seau *)
val moyenne_des_canaux : int -> int -> in_channel -> float

(** � partir d'un nom de fichier, renvoie une queue des valeurs contenues dans
   un fichier wav ainsi que deux entiers qui ne servent que pour la
   programmation *)
val cr�ation_des_echantillons :
  string ->
  int * int *
  (Complex.t, Bigarray.complex64_elt, Bigarray.c_layout) Bigarray.Array1.t
  Queue.t

(** Renvoie les coefficients de la transform�e de fourier pour un fichier wav *)
val transform�e_de_fourier : string -> float array Queue.t

(** main peut-�tre utilis�e � la place de transform�e_de_fourier *)
val main : string -> float array Queue.t
