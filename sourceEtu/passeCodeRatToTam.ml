(* Module de génération du code *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = String =
struct

  open Tds
  open Exceptions
  open Ast
  open Type
  open AstPlacement

  type t1 = Ast.AstPlacement.programme
  type t2 = String

(* generation_code_expression : AstPlacement.expression -> String *)
(* Paramètre e : l'expression à analyser *)
(* Génère le code et renvoie un String *)
let rec generation_code_expression e =
  match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstPlacement.AppelFonction(ia, listExp) -> 
    let code_liste = List.fold_left generation_code_expression listExp "" in
    match info_ast_to_info ia with
    | InfoFun(nom,_,_,_) -> code_liste ^ "CALL (ST) " ^ nom ^ "\n"
    | _ -> failwith "Pas possible"
  (* Accès à un identifiant représenté par son nom *)
  | AstPlacement.Ident(ia) ->
    let adr = getAdresse ia in
    let t = getTaille ia in
      "LOAD " ^ string_of_int t ^ " " ^ adr ^ "\n" 
  (* Booléen *)
  | AstPlacement.Booleen(bool) ->
    if (bool) then 
      "LOAD 1\n"
    else
      "LOAD 0\n"
  (* Entier *)
  | AstPlacement.Entier(int) ->
    "LOAD " ^ string_of_int int ^ "\n"
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstPlacement.Unaire(un, expr) ->
    let code_de_e = generation_code_expression expr in
    match un with
    | Numerateur -> code_de_e ^ "POP (0) 1\n"
    | Denominateur -> code_de_e ^ "POP (1) 1\n"
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstPlacement.Binaire(bin, expression1, expression2) ->
    let code1 = generation_code_expression expression1 in
    let code2 = generation_code_expression expression2 in
    let code = code1 ^ "\n" ^ code2 ^ "\n" in
      match bin with
      | Fraction -> code
      | PlusInt -> code ^ "SUBR IAdd\n"
      | PlusRat -> code ^ "CALL (ST) RAdd\n"
      | MultInt -> code ^ "SUBR IMul\n"
      | MultRat -> code ^ "CALL (ST) RMul\n"
      | EquInt -> code ^ "SUBR IEq\n"
      | EquBool -> code ^ "SUBR IEq\n"
      | Inf -> code ^ "SUBR ILss\n"

(* generation_code_instruction : AstPlacement.instruction -> String *)
(* Paramètre i : l'instruction à analyser *)
(* Génère le code et renvoie un String *)
let rec generation_code_instruction i =
  match i with
  | AstPlacement.Declaration(ia, e) ->
    let t = getTaille ia in
    let code_e = generation_code_expression e in
      "PUSH " ^ t ^ "\n" ^ code_e ^ "STORE " ^ t ^ "\n"
  | AstPlacement.Affectation(ia, e) ->
    let t = getTaille ia in
    let adr = getAdresse ia in
    let code_e = generation_code_expression e in
      code_e ^ "STORE " ^ t ^ " " ^ adr ^ "\n" 
  | AstPlacement.AffichageInt(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "SUBR IOut"
  | AstPlacement.AffichageRat(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "CALL (ST) ROut"
  | AstPlacement.AffichageBool(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "SUBR BOut"
  | AstPlacement.Conditionnelle(c,t,e) ->
    let code_c = generation_code_expression c in
    let code_t = generation_code_bloc t in
    let code_e = generation_code_bloc e in
    let etiquette_sinon = getEtiquette in
    let etiquette_finsi = getEtiquette in
      code_c ^ "JUMPIF (0) " ^ etiquette_sinon ^ "\n" ^ 
      code_t ^ "JUMP " ^ etiquette_finsi ^ "\n" ^
      etiquette_sinon ^ "\n" ^ code_e ^ etiquette_finsi ^ "\n"
  | AstPlacement.TantQue(c,b) -> 
    let code_c = generation_code_expression c in
    let code_t = generation_code_bloc b in
    let etiquette_debut = getEtiquette in
    let etiquette_fin = getEtiquette in
      etiquette_debut ^ "\n" code_c ^ "JUMPIF (0) " ^ etiquette_fin ^ "\n" ^
      code_b ^ "JUMP " ^ etiquette_debut ^ "\n" etiquette_fin ^ "\n"
  | AstPlacement.Retour(e) ->
  | Empty -> ""

end