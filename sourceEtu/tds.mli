open Type 

(* Définition du type des informations associées aux identifiants *)
type info =
  (* Information associée à une constante : son nom (non indispensable mais aide au test et debbugage) et sa valeur *)
  | InfoConst of string * int
  (* Information associée à une variable : son nom (non indispensable mais aide au test et debbugage),
  son type, et son adresse ie son déplacement (int) par rapport à un registre (string) *)
  | InfoVar of string * typ * int * string
  (* Information associée à une fonction : son nom (utile pour l'appel), son type de retour et la liste des types des paramètres *)
  | InfoFun of string * typ * typ list
  (* Information associée à un type crée *)
  | InfoType of string * typ
  (* Information associée à un enregistrement crée *)
  | InfoEnre of string * typ list * info_ast list * int * int * string
(* Données stockées dans la tds et dans les AST : pointeur sur une information *)
and info_ast
(* Table des symboles *)
type tds 



(* Création d'une table des symboles à la racine *)
val creerTDSMere : unit -> tds 

(* Création d'une table des symboles fille *)
(* Le paramètre est la table mère *)
val creerTDSFille : tds -> tds 

(* Ajoute une information dans la table des symboles locale *)
(* tds : la tds courante *)
(* string : le nom de l'identificateur *)
(* info : l'information à associer à l'identificateur *)
(* Si l'identificateur est déjà présent dans TDS, l'information est écrasée *)
(* retour : unit *)
val ajouter : tds -> string -> info_ast -> unit 

(* Recherche les informations d'un identificateur dans la tds locale *)
(* Ne cherche que dans la tds de plus bas niveau *)
val chercherLocalement : tds -> string -> info_ast option 

(* Recherche les informations d'un identificateur dans la tds globale *)
(* Si l'identificateur n'est pas présent dans la tds de plus bas niveau *)
(* la recherche est effectuée dans sa table mère et ainsi de suite *)
(* jusqu'à trouver (ou pas) l'identificateur *)
val chercherGlobalement : tds -> string -> info_ast option 

(* Affiche la tds locale *)
val afficher_locale : tds -> unit 

(* Affiche la tds locale et récursivement *)
val afficher_globale : tds -> unit 

(* Créer une information à associer à l'AST à partir d'une info *)
val info_to_info_ast : info -> info_ast

(* Récupère l'information associée à un noeud *)
val info_ast_to_info : info_ast -> info

(* Modifie le type si c'est une InfoVar, ne fait rien sinon *)
val modifier_type_info : typ -> info_ast -> unit

(* Modifie le type si c'est une InfoEnre, ne fait rien sinon *)
val modifier_type_info_enre : typ list -> info_ast -> unit

(* Modifie les types de retour et des paramètres si c'est une InfoFun, ne fait rien sinon *)
val modifier_type_fonction_info : typ -> typ list -> info_ast -> unit

(* Modifie l'emplacement (dépl, registre) si c'est une InfoVar, ne fait rien sinon *)
val modifier_adresse_info : int -> string -> info_ast -> unit

(* Modifie l'emplacement (dépl,registre) si c'est un enregistrement *)
val modifier_adresse_enre_ia : int -> string -> info_ast list -> unit

(* Modifie la taille de l'enregistrement *)
val modifier_taille_enregistrement : int -> info_ast -> unit

(* Renvoie le type d'une info ast *)
val getType : info_ast -> typ

(* Renvoie le type d'une info ast *)
val getTaille : info_ast -> int

(* Renvoie le type d'une info ast *)
val getAdresse: info_ast -> string

(* Renvoie le type d'une info ast *)
val getTaillePara: info_ast -> int

val getNom: info_ast -> string
