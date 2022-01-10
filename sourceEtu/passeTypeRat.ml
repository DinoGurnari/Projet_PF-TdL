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


let rec analyse_type_affectation a = 
  match a with 
  | AstTds.Ident(ia) ->
    (AstType.Ident(ia), getType ia)
  | AstTds.Deref aff ->
    let (na,typ) = analyse_type_affectation aff in
      begin
      match typ with
      | Adr(t) ->
        (Deref(na),t)
      | _ ->
        raise (TypeNonDeferencable(typ))
      end 
  |AstTds.Champ(aff,ia) ->
  failwith "pascoder"


(* analyse_type_expression : AstTds.expression -> (AstType.expression * typ) *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des types et tranforme l'expression
en une expression de type AstType.expression *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstTds.AppelFonction(ia, listExp) -> 
    let listExpN = List.map analyse_type_expression listExp in
    let listExpSansTypes = List.map (fun (x,_) -> x) listExpN in
    let listTypesExp = List.map (fun (_,y) -> y) listExpN in
    let info = info_ast_to_info ia in
      begin
      match info with
      | InfoFun (_, _, listTypesPara) -> 
        let egalite = List.equal (fun x y -> x = y) listTypesExp listTypesPara in
        if egalite then
          (AstType.AppelFonction(ia, listExpSansTypes), getType ia)
        else
          raise (TypesParametresInattendus (listTypesExp, listTypesPara))
      | _ -> failwith "Erreur interne : Pas possible"
      end
  (* Booléen *)
  | AstTds.Booleen(bool) ->
    (AstType.Booleen(bool), Bool)
  (* Entier *)
  | AstTds.Entier(int) ->
    (AstType.Entier(int), Int)
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstTds.Unaire(un, expr) ->
    let (ne, te) = analyse_type_expression expr in
      begin
      match te with
      | Rat -> (AstType.Unaire(un, ne), Int)
      | _ -> raise (TypeInattendu (te, Rat))
      end
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstTds.Binaire(bin, expression1, expression2) ->
    let (ne1, te1) = analyse_type_expression expression1 in
    let (ne2, te2) = analyse_type_expression expression2 in
        begin
        match bin with
        | Fraction -> 
          if (te1 = Int && te2 = Int) then
            (AstType.Binaire(Fraction, ne1, ne2), Rat)
          else 
            raise (TypeBinaireInattendu(bin, te1, te2))
        | Plus -> 
          begin
          match te1,te2 with
          | Int,Int ->
            (AstType.Binaire(PlusInt, ne1, ne2), Int)
          | Rat,Rat ->
            (AstType.Binaire(PlusRat, ne1, ne2), Rat)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Mult ->
          begin
          match te1,te2 with
          | Int,Int ->
            (AstType.Binaire(MultInt, ne1, ne2), Int)
          | Rat,Rat ->
            (AstType.Binaire(MultRat, ne1, ne2), Rat)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Equ ->
          begin
          match te1,te2 with
          
          | Int,Int ->
            (AstType.Binaire(EquInt, ne1, ne2), Bool)
          | Bool,Bool ->
            (AstType.Binaire(EquBool, ne1, ne2), Bool)
          | _ ->
            raise (TypeBinaireInattendu(bin, te1, te2))
          end
        | Inf ->
          if (te1 = Int && te2 = Int) then
            (AstType.Binaire(Inf, ne1, ne2), Bool)
          else 
            raise (TypeBinaireInattendu(bin, te1, te2))
        end
  | AstTds.Affectation(aff) ->
    let (na,typ) = analyse_type_affectation aff in
      (AstType.Affectation(na),typ)
  | AstTds.Null ->
    (AstType.Null, Type.Null)

  | AstTds.New(typ) ->
    (AstType.New , Adr(typ))

  | AstTds.Adr(ia) ->
    (AstType.Adr(ia), Adr(getType ia))
  | AstTds.Enre(le) ->
    let len = List.map analyse_type_expression le in
    let listTypesExp = List.map (fun (_,y) -> y) len in
      failwith "pascoder"

        

(* analyse_type_instruction : AstTds.instruction -> AstType.instruction *)
(* Paramètre i : l'instruction à analyser *)
(* Paramètre tf : type de retour de la fonction (si elle existe, sinon nul) *)
(* Vérifie la bonne utilisation des types et tranforme l'instruction
en une instruction de type AstType.instruction *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction tf i =
  match i with
  | AstTds.Declaration (t, ia, e) ->
    modifier_type_info t ia;
    let (ne, te) = analyse_type_expression e in
      if est_compatible t te then 
        AstType.Declaration(ia, ne)
      else
        raise (TypeInattendu (te, t))

  | AstTds.Affectation (aff, e) ->
    let (na, ta) = analyse_type_affectation aff in
    let (ne, te) = analyse_type_expression e in
      if est_compatible ta te then 
        AstType.Affectation(na, ne)
      else
        raise (TypeInattendu (te, ta))

  | AstTds.Affichage e -> 
    let (ne, te) = analyse_type_expression e in
      begin
      match te with
      | Int -> AffichageInt ne
      | Rat -> AffichageRat ne
      | Bool -> AffichageBool ne
      | Adr _ -> AffichageInt ne
      | tr -> raise (TypeInattendu (tr, te))
      end

  | AstTds.Conditionnelle (c,t,e) -> 
    let (nc, te) = analyse_type_expression c in
      if te != Bool then
        raise (TypeInattendu (te, Bool))
      else
        let nt = analyse_type_bloc tf t in
        let ne = analyse_type_bloc tf e in
          Conditionnelle(nc, nt, ne)
  | AstTds.TantQue (c,b) -> 
    let (nc, te) = analyse_type_expression c in
      if te != Bool then
        raise (TypeInattendu (te, Bool))
      else 
        let nb = analyse_type_bloc tf b in
        TantQue (nc, nb)
  | AstTds.Retour (e) -> 
    let (ne, te) = analyse_type_expression e in
      begin
      match tf with
      | None -> raise (RetourDansMain)
      | Some t ->  
        if est_compatible t te then
          Retour(ne)
        else
          raise (TypeInattendu(te, t))
      end
  | AstTds.Empty ->
    AstType.Empty
          
(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le bloc
en un bloc de type AstType.bloc *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc tf li =
  let nli =List.map (analyse_type_instruction tf) li in 
  nli

(* analyse_tds_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des types et tranforme la fonction
en une fonction de type AstType.fonction *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction(t,ia,lp,li))  =
  let analyser_type_para (t, ia) =
    modifier_type_info t ia;
    ia
  in
  let nlp = List.map analyser_type_para lp in
  let listeTypes = List.map (fun (x, _) -> x) lp in
    modifier_type_fonction_info t listeTypes ia;
    let nli = analyse_type_bloc (Some t) li in
      AstType.Fonction(ia, nlp, nli)


(* analyser : AstTds.ast -> AstType.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des types et tranforme le programme
en un programme de type AstType.ast *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (fonctions,prog)) =
  let nf = List.map analyse_type_fonction fonctions in 
  let nb = analyse_type_bloc None prog in
  Programme (nf,nb)  

end