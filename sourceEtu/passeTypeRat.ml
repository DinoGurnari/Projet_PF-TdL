(* Module de la passe de gestion des types *)
module PasseTypeRat : Passe.Passe with type t1 = Ast.AstTds.programme and type t2 = Ast.AstType.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstType
  open Type

  type t1 = Ast.AstTds.programme
  type t2 = Ast.AstType.programme

(* analyse_type_expression : AstTds.expression -> AstType.expression *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstTds.AppelFonction(ia, listExp) -> 
    let listExpN = List.map analyse_type_expression listExp in
      (AstType.AppelFonction(ia, listExpN), getType ia)
  (* Accès à un identifiant représenté par son nom *)
  | AstTds.Ident(ia) ->
    (AstType.Ident(ia), getType ia)
  (* Booléen *)
  | AstTds.Booleen(bool) ->
    (AstType.Booleen(bool), Bool)
  (* Entier *)
  | AstTds.Entier(int) ->
    (AstType.Entier(int), Int)
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstTds.Unaire(un, expr) ->
    let (ne, te) = analyse_type_expression expr in
      match te with
      | Rat -> (AstType.Unaire(un, ne), Int)
      | _ -> raise (TypeInattendu (Rat, te))
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstTds.Binaire(bin, expression1, expression2) ->
    let (ne1, te1) = analyse_type_expression expression1 in
    let (ne2, te2) = analyse_type_expression expression2 in
      if (te1 != te2) then 
        raise (TypeBinaireInattendu (bin, te1, te2))
      else
        match bin with
        | Fraction -> 
          if (te1 = Int) then
            (AstType.Binaire(Fraction, ne1, ne2), Rat)
          else 
            raise (TypeBinaireInattendu(bin, te1, te2))
        | Plus -> 
          begin
          match te1 with
          | Int ->
            (AstType.Binaire(PlusInt, ne1, ne2), Int)
          | Rat ->
            (AstType.Binaire(PlusRat, ne1, ne2), Rat)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Mult ->
          begin
          match te1 with
          | Int ->
            (AstType.Binaire(MultInt, ne1, ne2), Int)
          | Rat ->
            (AstType.Binaire(MultRat, ne1, ne2), Rat)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Equ ->
          begin
          match te1 with
          | Int ->
            (AstType.Binaire(EquInt, ne1, ne2), Bool)
          | Bool ->
            (AstType.Binaire(EquBool, ne1, ne2), Bool)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Inf ->
          if (te1 = Int) then
            (AstType.Binaire(Inf, ne1, ne2), Bool)
          else 
            raise (TypeBinaireInattendu(bin, te1, te2))

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre tf : type de retour de la fonction (si elle existe, sinon nul) *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i tf =
  match i with
  | AstSyntax.Declaration (t, ia, e) ->
    modifier_type_info t ia;
    let (ne, te) = analyse_type_expression e in
      if t = te then 
        AstType.Declaration(ia, ne)
      else
        raise (TypeInattendu (te, t))

  | AstSyntax.Affectation (ia, e) ->
    let t = getType ia in
    let (ne, te) = analyse_type_expression e in
      if t = te then 
        AstType.Affectation(ia, ne)
      else
        raise (TypeInattendu (te, t))

  | AstSyntax.Affichage e -> 
    let (ne, te) = analyse_type_expression e in
      match te with
      | Int -> AffichageInt ne
      | Rat -> AffichageRat ne
      | Bool -> AffichageBool ne

  | AstSyntax.Conditionnelle (c,t,e) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) -> 
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds b in
      (* Renvoie la nouvelle structure de la boucle *)
      TantQue (nc, bast)
  | AstSyntax.Retour (e) -> 
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      Retour (ne)


end