(** Permet de "parser" facilement les "headers" de flux de données
   binaires (c'est-à-dire non texte) que l'on trouve dans les fichiers et plus
   généralement tous les canaux de communication (type in_channel d'ocaml).
   Là bibliothèque utilise une liste dite de parsing, chaque triplet de la liste
   (triplet de parsing) contenant une fonction de lecture, un identifiant et une
   fonction permettant d'étendre dynamiquement à l'éxécution le parsing, utile
   lorsque le header ne peut être déterminé à la compilation. *) 

(** Exception soulevée lorsque que l'on essaie de lire un entier stocké sur
   autre chose que 8, 16 ou 32 bits. Les ajouts sont faciles mais il faut
   ensuite pouvoir stocker les données dans les entiers/int d'ocaml ; au-delà il
   faut des progcesseurs 64 bits ou des floats. *)
exception A_implementer of string

(** On a recours au polymorphisme car les headers peuvent contenir tout autant
   des chaînes de caractères que des nombres. *)
type value = S of string | I of int

(** Le type pour les fonctions génératrices. À partir de l'ensemble des valeurs
   du header (stockées dans la table de hachage en argument), ces fonctions
   génèrent une liste de parsing. *)
type gen_fct =
    (string, value) Hashtbl.t ->
    ((in_channel -> value) * string * gen_fct option) list

(** La fonction principale du module, prend en argument un canal de lecture puis
   une liste de parsing. En général, cet élément vaut None. Si une fonction est
   présente, elle est appelée immédiatement avec comme argument la table de
   hachage dont le dernier ajout correspond au triplet de parsing d'où provient
   la fonction ; la liste générée est "insérée" dans la liste de parsing
   générale.
   Les valeurs sont stockées dans une table de hachage avec comme identifiant la
   chaîne de caractères fournie en argument. *)
val header_parse :
  in_channel ->
  ((in_channel -> value) * string * gen_fct option) list ->
  in_channel * (string, value) Hashtbl.t

(** Cette fonction sert à lire les entiers stockés en little endian et prennent
   en premier argument le nombre d'octets à lire. L'application partielle de
   cette fonction avec son premier argument constitue le premier élément du
   triplet renvoyé par les fonctions de type gen_fct. *)
val little_endian_read_ui8n : int -> in_channel -> value

(** Fonction identique à la précédente exceptée qu'elle sert à lire des entiers
   stockés en big endian. *)
val big_endian_read_ui8n : int -> in_channel -> value

(** Permet la lecture d'une chaîne de caractère de longueur, le premier argument
   est la longueur de celle-ci. *)
val input_string : int -> in_channel -> value

