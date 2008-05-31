(** Permet de "parser" facilement les "headers" de flux de donn�es
   binaires (c'est-�-dire non texte) que l'on trouve dans les fichiers et plus
   g�n�ralement tous les canaux de communication (type in_channel d'ocaml).
   L� biblioth�que utilise une liste dite de parsing, chaque triplet de la liste
   (triplet de parsing) contenant une fonction de lecture, un identifiant et une
   fonction permettant d'�tendre dynamiquement � l'�x�cution le parsing, utile
   lorsque le header ne peut �tre d�termin� � la compilation. *) 

(** Exception soulev�e lorsque que l'on essaie de lire un entier stock� sur
   autre chose que 8, 16 ou 32 bits. Les ajouts sont faciles mais il faut
   ensuite pouvoir stocker les donn�es dans les entiers/int d'ocaml ; au-del� il
   faut des progcesseurs 64 bits ou des floats. *)
exception A_implementer of string

(** On a recours au polymorphisme car les headers peuvent contenir tout autant
   des cha�nes de caract�res que des nombres. *)
type value = S of string | I of int

(** Le type pour les fonctions g�n�ratrices. � partir de l'ensemble des valeurs
   du header (stock�es dans la table de hachage en argument), ces fonctions
   g�n�rent une liste de parsing. *)
type gen_fct =
    (string, value) Hashtbl.t ->
    ((in_channel -> value) * string * gen_fct option) list

(** La fonction principale du module, prend en argument un canal de lecture puis
   une liste de parsing. En g�n�ral, cet �l�ment vaut None. Si une fonction est
   pr�sente, elle est appel�e imm�diatement avec comme argument la table de
   hachage dont le dernier ajout correspond au triplet de parsing d'o� provient
   la fonction ; la liste g�n�r�e est "ins�r�e" dans la liste de parsing
   g�n�rale.
   Les valeurs sont stock�es dans une table de hachage avec comme identifiant la
   cha�ne de caract�res fournie en argument. *)
val header_parse :
  in_channel ->
  ((in_channel -> value) * string * gen_fct option) list ->
  in_channel * (string, value) Hashtbl.t

(** Cette fonction sert � lire les entiers stock�s en little endian et prennent
   en premier argument le nombre d'octets � lire. L'application partielle de
   cette fonction avec son premier argument constitue le premier �l�ment du
   triplet renvoy� par les fonctions de type gen_fct. *)
val little_endian_read_ui8n : int -> in_channel -> value

(** Fonction identique � la pr�c�dente except�e qu'elle sert � lire des entiers
   stock�s en big endian. *)
val big_endian_read_ui8n : int -> in_channel -> value

(** Permet la lecture d'une cha�ne de caract�re de longueur, le premier argument
   est la longueur de celle-ci. *)
val input_string : int -> in_channel -> value

