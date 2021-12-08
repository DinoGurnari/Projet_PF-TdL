(* Module de la passe de placement mémoire *)
module PassePlacementRat : Passe.Passe with type t1 = Ast.AstType.programme and type t2 = Ast.AstPlacement.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstType
  open Type
  open AstPlacement

  type t1 = Ast.AstType.programme
  type t2 = Ast.AstPlacement.programme

(* analyse_placement_expression : AstType.expression -> AstPlacement.expression *)
(* Paramètre e : l'expression à analyser *)
(* Place la mémoire et renvoie une AstPlacement.expression *)
let rec analyse_placement_expression e =
  e

(* analyse_placement_instruction : AstType.instruction -> AstPlacement.instruction *)
(* Paramètre reg : registre à utiliser *)
(* Paramètre dep : déplacement dans la mémoire *)
(* Paramètre i : l'instruction à analyser *)
(* Place la mémoire et renvoie une AstPlacement.instruction et le déplacement *)
let rec analyse_placement_instruction reg dep i =
  match i with
  | AstType.Declaration (ia, e) ->
    modifier_adresse_info dep reg ia;
    let t = getType ia in
      begin
      match t with
      | Rat -> (i, dep + 2)
      | _ -> (i, dep + 1)
      end

  | AstType.Conditionnelle (c,t,e) -> 
    let _ = analyse_placement_bloc reg dep t in
    let _ = analyse_placement_bloc reg dep e in
      (i, dep)

  | AstType.TantQue (c,b) -> 
    let _ = analyse_placement_bloc reg dep b in
      (i, dep)

  | _ -> (i, dep)

(* analyse_placement_bloc : AstType.bloc -> AstPlacement.bloc *)
(* Paramètre reg : registre à utiliser *)
(* Paramètre dep : déplacement dans la mémoire *)
(* Paramètre li : liste d'instructions à analyser *)
(* Place la mémoire et renvoie une AstPlacement.bloc *)
and analyse_placement_bloc reg dep li =
  match li with
  | [] -> []
  | i::q -> 
    let (ni, ndep) = analyse_placement_instruction reg dep i in
      ni::(analyse_placement_bloc reg ndep q)

let rec analyse_placement_param dep rlp = 
  match rlp with
    | [] -> []
    | ia :: q -> 
      let getTaille  ia = 
        let typ = getType ia in
        match typ with
          | Rat -> 2
          | _ -> 1
      in
        let d = getTaille ia in 
          modifier_adresse_info (dep - d) "LB" ia;
          ia::(analyse_placement_param (dep - d) q)

(* analyse_tds_fonction : AstType.fonction -> AstPlacement.fonction *)
(* Paramètre : la fonction à analyser *)
(* Place la mémoire et renvoie une AstPlacement.Fonction *)
let analyse_placement_fonction (AstType.Fonction(ia,lp,li))  =
  let nb = analyse_placement_bloc "LB" 3 li in 
  let nlp = analyse_placement_param 0 (List.rev lp) in
  AstPlacement.Fonction(ia,nlp, nb)

(* analyser : AstType.ast -> AstPlacement.ast *)
(* Paramètre : le programme à analyser *)
(* Place la mémoire et renvoie une AstPlacement.bloc *)
let analyser (AstType.Programme (fonctions,prog)) =
  let nf = List.map analyse_placement_fonction fonctions in 
  let nb = analyse_placement_bloc "SB" 0 prog in
  Programme (nf,nb)  

end