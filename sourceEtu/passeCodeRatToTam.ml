(* Module de génération du code *)
module PasseCodeRatToTam : Passe.Passe with type t1 = Ast.AstPlacement.programme and type t2 = string =
struct

  open Tds
  open Ast
  open Type

  type t1 = Ast.AstPlacement.programme
  type t2 = string

(* generation_code_expression : AstPlacement.expression -> String *)
(* Paramètre e : l'expression à analyser *)
(* Génère le code et renvoie un String *)
let rec generation_code_expression e =
  match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstType.AppelFonction(ia, listExp) -> 
    let code_liste = List.fold_left (fun a b -> a ^ generation_code_expression b) "" listExp in
    begin
    match info_ast_to_info ia with
    | InfoFun(nom,_,_) -> code_liste ^ "CALL (SB) " ^ nom ^ "\n"
    | _ -> failwith "Pas possible"
    end
  (* Accès à un identifiant représenté par son nom *)
  | AstType.Ident(ia) ->
    let adr = getAdresse ia in
    let t = Tds.getTaille ia in
      "LOAD (" ^ string_of_int t ^ ") " ^ adr ^ "\n" 
  (* Booléen *)
  | AstType.Booleen(bool) ->
    if (bool) then 
      "LOADL 1\n"
    else
      "LOADL 0\n"
  (* Entier *)
  | AstType.Entier(int) ->
    "LOADL " ^ string_of_int int ^ "\n"
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstType.Unaire(un, expr) ->
    let code_de_e = generation_code_expression expr in
    begin
    match un with
    | Numerateur -> code_de_e ^ "POP (0) 1\n"
    | Denominateur -> code_de_e ^ "POP (1) 1\n"
    end
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstType.Binaire(bin, expression1, expression2) ->
    let code1 = generation_code_expression expression1 in
    let code2 = generation_code_expression expression2 in
    let code = code1 ^ "\n" ^ code2 ^ "\n" in
      begin
      match bin with
      | Fraction -> code
      | PlusInt -> code ^ "SUBR IAdd\n"
      | PlusRat -> code ^ "CALL (SB) RAdd\n"
      | MultInt -> code ^ "SUBR IMul\n"
      | MultRat -> code ^ "CALL (SB) RMul\n"
      | EquInt -> code ^ "SUBR IEq\n"
      | EquBool -> code ^ "SUBR IEq\n"
      | Inf -> code ^ "SUBR ILss\n"
      end

 

(* generation_code_instruction : AstPlacement.instruction -> String *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre ia : l'info ast correspondant la fonction appellée (si elle existe sinon none)*)
(* Génère le code et renvoie un String *)
let rec generation_code_instruction ia i =
  match i with
  | AstType.Declaration(ia, e) ->
    let t = Tds.getTaille ia in
    let adr = Tds.getAdresse ia in
    let code_e = generation_code_expression e in
      "PUSH " ^ string_of_int t ^ "\n" ^ code_e ^ "STORE (" ^ string_of_int t ^ ") " ^ adr ^ "\n"
  | AstType.Affectation(ia, e) ->
    let t = Tds.getTaille ia in
    let adr = Tds.getAdresse ia in
    let code_e = generation_code_expression e in
      code_e ^ "STORE (" ^ string_of_int t ^ ") " ^ adr ^ "\n" 
  | AstType.AffichageInt(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "SUBR IOut\n" 
  | AstType.AffichageRat(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "CALL (SB) ROut\n"
  | AstType.AffichageBool(e) ->
    let code_e = generation_code_expression e in
      code_e ^ "SUBR BOut\n"
  | AstType.Conditionnelle(c,t,e) ->
    let code_c = generation_code_expression c in
    let code_t = generation_code_bloc ia t in
    let code_e = generation_code_bloc ia e in
    let etiquette_sinon = Code.getEtiquette() in
    let etiquette_finsi = Code.getEtiquette() in
      code_c ^ "JUMPIF (0) " ^ etiquette_sinon ^ "\n" ^ 
      code_t ^ "JUMP " ^ etiquette_finsi ^ "\n" ^
      etiquette_sinon ^ "\n" ^ code_e ^ etiquette_finsi ^ "\n"
  | AstType.TantQue(c,b) -> 
    let code_c = generation_code_expression c in
    let code_t = generation_code_bloc ia b in
    let etiquette_debut = Code.getEtiquette() in
    let etiquette_fin = Code.getEtiquette() in
      etiquette_debut ^ "\n" ^ code_c ^ "JUMPIF (0) " ^ etiquette_fin ^ "\n" ^
      code_t ^ "JUMP " ^ etiquette_debut ^ "\n" ^ etiquette_fin ^ "\n"
  | AstType.Retour(e) -> 
      let code_e = generation_code_expression e in 
      begin
      match ia with 
      | None -> 
        failwith "impossible"

      | Some iaf ->
        let t = Tds.getTaille iaf in 
        let tlp = Tds.getTaillePara iaf in
          code_e ^ "RETURN (" ^ string_of_int t ^ ") " ^ string_of_int tlp ^ "\n" 
      end
  | Empty -> ""

(* generation_code_bloc : AstPlacement.bloc -> string *)

(* Paramètre li : liste d'instructions à analyser *)
(* Génère le code et renvoie un String *)
and generation_code_bloc ia li =
  let isDeclaration b =
    match b with 
    | AstType.Declaration(ia,_) ->
      let t = getType ia in
      begin
      match t with 
      | Rat ->
        2
      | _ -> 
        1
      end
    | _ -> 
      0
    in
    let (code_b,nbvarlocale) = List.fold_left (fun (code,nbvar) b -> (code ^ (generation_code_instruction ia b),nbvar + isDeclaration b)) ("",0) li in
      code_b ^ "POP (0) " ^ string_of_int nbvarlocale ^ "\n"


(* generation_code_fonction : AstPlacement.fonction -> String *)

(* Paramètre : la fonction à analyser *)
(* Génère le code et renvoie un String *)
let generation_code_fonction (AstPlacement.Fonction(ia,_,li))  = 
  let code_li = generation_code_bloc (Some ia) li in
  match info_ast_to_info ia with
    | InfoFun(nom,_,_) -> 
      nom ^ "\n" ^ code_li ^ "\nHALT\n\n"
    | _ -> failwith "Pas possible"
  

(* analyser : AstPlacement.ast -> String *)
(* Paramètre : le programme à analyser *)
(* Génère le code et renvoie un String *)
let analyser (AstPlacement.Programme (fonctions,prog)) =
  let code_f = List.fold_left (fun a b -> a ^ generation_code_fonction b) "" fonctions in
  let code_b = generation_code_bloc None prog in 
  let entete = Code.getEntete() in
    entete ^ code_f ^ "main\n" ^ code_b ^ "\nHALT"

end