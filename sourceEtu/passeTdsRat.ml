(* Module de la passe de gestion des identifiants *)
module PasseTdsRat : Passe.Passe with type t1 = Ast.AstSyntax.programme and type t2 = Ast.AstTds.programme =
struct

  open Tds
  open Exceptions
  open Ast
  open AstTds
  open Type

  type t1 = Ast.AstSyntax.programme
  type t2 = Ast.AstTds.programme
(** renvoie l'info ast d'un affectable *)
let rec get_ia_affectable a = 
  match a with
  | Deref(aff) ->  get_ia_affectable aff
  | Ident(ia) -> ia
  | Champ(aff,n) -> get_ia_champ (get_ia_affectable aff) (getNom n) true
and 
  get_ia_champ ia n bool =
  match info_ast_to_info ia with
  | InfoEnre(nomenre,_,ial,_,_,_) ->
    
    let rec get_ia ialist id =
    begin
    match ialist with
    | [] -> failwith "n pas champs"
    | ia::q -> 
      let nom = getNom ia in
        if (bool) then
          
          if String.equal nom (id) then
            ia
          else 
            get_ia q id 
        
        else
        if String.equal nom (nomenre^"."^id) then
          ia
        else 
          get_ia q id    
    end
    in
    get_ia ial n
  | _ -> failwith "mauvais type"

(* renvoie le type réel d'un type *)
let rec analyse_tds_type tds typ =
  match typ with
    | Tid(tid) -> 
      let sia = chercherGlobalement tds tid in
        begin
        match sia with
          | None -> raise (TypeNonDeclare tid)
          | Some ia ->
            begin
            match info_ast_to_info ia with
              | InfoType(_,t) -> t
              | _ -> raise (MauvaiseUtilisationIdentifiant tid)
            end
        end
    | Record pl ->
    (* ajouter les champs dans la tds   *)
      let ajouter_champs (typage, x) =
        begin
        match chercherLocalement tds x with            
        | None ->
        let info = InfoVar (x,Undefined, 0, "") in
        let ia = info_to_info_ast info in
        ajouter tds x ia;
        let t = analyse_tds_type tds typage in
        modifier_type_info t ia;
        ia
        | Some _ ->
        raise (DoubleDeclaration x)
        end
        in
        let _ = List.map (ajouter_champs) pl in
        let npl = List.map (fun (t,x) -> (analyse_tds_type tds t,x)) pl in
          Record(npl)
        
    | _ -> typ
  (* analyse de définition d'un type *)
let analyse_tds_deftype tds (AstSyntax.Typedef(tid,typ)) = 
  match Tds.chercherLocalement tds tid with
    | Some _ -> raise (DoubleDeclaration tid)
    | None ->
      let t = analyse_tds_type tds typ in
      let info = InfoType(tid,t) in
      let ia = info_to_info_ast info in
        ajouter tds tid ia
        


let rec analyse_tds_affectation a modife tds =

  match a with 
  | AstSyntax.Ident(id) ->
    begin
    match Tds.chercherGlobalement tds id with
    | None -> raise (IdentifiantNonDeclare id)
    | Some ia -> 
      begin
      match info_ast_to_info ia with 
      | InfoFun _ -> 
        raise (MauvaiseUtilisationIdentifiant id)
      | InfoType _ -> 
        raise (MauvaiseUtilisationIdentifiant id)
      | InfoConst(id,_) -> 
        begin
        if modife then
          raise (MauvaiseUtilisationIdentifiant id)
        else
          AstTds.Ident(ia)
        end
      | InfoVar _ -> 
        AstTds.Ident(ia)
      | InfoEnre _ ->
        AstTds.Ident(ia)
        

      end
    end
    (* déréférencement d'un pointeur *)
  | AstSyntax.Deref aff -> 
    let na =analyse_tds_affectation aff modife tds in
    AstTds.Deref(na)
    (*Acces à un champs *)
  | AstSyntax.Champ(aff,n) ->
    let na =analyse_tds_affectation aff modife tds in
        let iaaff= get_ia_affectable na in
        let lerucarenvoyer = get_ia_champ iaaff n false in
        Champ(na,lerucarenvoyer)
        
   



(* analyse_tds_expression : AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
  match e with
  (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
  | AstSyntax.AppelFonction(id, listExp) -> 
    begin
    match Tds.chercherGlobalement tds id with
    | None -> raise (IdentifiantNonDeclare id)
    | Some ia -> 
      begin
      match (info_ast_to_info ia) with 
      | InfoFun _ ->
        let liste = List.map(analyse_tds_expression tds) listExp in
          AstTds.AppelFonction(ia, liste)
      | _ -> raise (MauvaiseUtilisationIdentifiant id)
      end
    end
  (* Accès à un identifiant représenté par son nom *)
  
  (* Booléen *)
  | AstSyntax.Booleen(bool) ->
    AstTds.Booleen(bool)
  (* Entier *)
  | AstSyntax.Entier(int) ->
    AstTds.Entier(int)
  (* Opération unaire représentée par l'opérateur et l'opérande *)
  | AstSyntax.Unaire(un, expr) ->
    AstTds.Unaire(un, analyse_tds_expression tds expr)
  (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
  | AstSyntax.Binaire(bin, expression1, expression2) ->
    AstTds.Binaire(bin, analyse_tds_expression tds expression1, analyse_tds_expression tds expression2)
  | AstSyntax.Affectation(aff) ->
    let na = analyse_tds_affectation aff false tds in
      AstTds.Affectation(na)
  | AstSyntax.Null ->
    AstTds.Null
  | AstSyntax.New(t) ->
    let typ = analyse_tds_type tds t in
    AstTds.New(typ)
  | AstSyntax.Adr(id) ->
    begin
    match Tds.chercherGlobalement tds id with
    | None -> raise (IdentifiantNonDeclare id)
    | Some ia -> 
      begin 
      match info_ast_to_info ia with
      | InfoVar _ -> AstTds.Adr(ia)
      | _ -> raise (MauvaiseUtilisationIdentifiant id) 
      end 
    end
    (* création d'un enregistrement *)
  | AstSyntax.Enre(le) ->
    let nle = List.map (analyse_tds_expression tds) le in
    
      AstTds.Enre(nle)




(* analyse_tds_instruction : AstSyntax.instruction -> tds -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale, 
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *) 
            let ne = analyse_tds_expression tds e in
            let typ = analyse_tds_type tds t in
            (* Création de l'information associée à l'identfiant *)
            
            let info = 
              begin 
              match typ with
                (* si c'est un enregistrement on doit aussi ajouter les champs *)
                | Type.Record(lc) ->
                  let tdsenre = creerTDSFille tds in
                  let ajouter_champs (typage, x) =
                    begin
                    match chercherLocalement tds x with            
                    | None ->
                    let nom = n^"."^x in
                    let info = InfoVar (nom,Undefined, 0, "") in
                    let ia = info_to_info_ast info in
                    ajouter tdsenre nom ia;
                    let t = analyse_tds_type tds typage in
                    modifier_type_info t ia;
                    ia
                    | Some _ ->
                    raise (DoubleDeclaration x)
                    end
                    in
                    let nlc = List.map (ajouter_champs) lc in
                  InfoEnre (n,[],nlc,0,0,"")
                  
                | _ ->
                  InfoVar (n,Undefined, 0, "")
              end  
            in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information 
            et l'expression remplacée par l'expression issue de l'analyse *)
            
            Declaration (typ, ia, ne) 
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale, 
            il a donc déjà été déclaré dans le bloc courant *) 
            raise (DoubleDeclaration n)
      end
      (*affectation à une variable *)
  | AstSyntax.Affectable (aff,e) ->
    let na = analyse_tds_affectation aff true tds in
    let ne = analyse_tds_expression tds e in
    AstTds.Affectation (na,ne)
  | AstSyntax.Constante (n,v) -> 
      begin
        match chercherLocalement tds n with
        | None -> 
        (* L'identifiant n'est pas trouvé dans la tds locale, 
        il n'a donc pas été déclaré dans le bloc courant *)
        (* Ajout dans la tds de la constante *)
        ajouter tds n (info_to_info_ast (InfoConst (n,v))); 
        (* Suppression du noeud de déclaration des constantes devenu inutile *)
        Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale, 
          il a donc déjà été déclaré dans le bloc courant *) 
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e -> 
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      Affichage (ne)
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
      (* Assignation plus : on renvoit un binaire qui correspond à l'assignation voulue  *)
  | AstSyntax.AssignationPlus (a,e) ->
      let na = analyse_tds_affectation a true tds in
      let ne = analyse_tds_expression tds e in
        AstTds.Affectation(na,AstTds.Binaire(Plus,AstTds.Affectation(na),ne)) 
  | AstSyntax.DeclarationType (deftype) ->
      analyse_tds_deftype tds deftype;
        Empty
      

      
(* analyse_tds_bloc : AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc
en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale 
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc 
  Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli


(* analyse_tds_fonction : AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction(t,n,lp,li))  =
  match chercherLocalement maintds n with
  | Some _ -> raise (DoubleDeclaration n)
  | None ->
    let infof = InfoFun(n,Undefined, []) in
    let ia = info_to_info_ast infof in 
      ajouter maintds n ia;
    let tdsbloc = creerTDSFille maintds in
      let ajouter_para (typage, x) =   
        match chercherLocalement tdsbloc x with
        | None ->
          begin
          let typ = analyse_tds_type maintds typage in 
          match typ with
          (* si c'est un enregistrement on doit aussi ajouter les champs *)
          | Record(lc) ->
            
            let tdsenre = creerTDSFille maintds in
                  let ajouter_champs (typage, x) =
                    begin
                    match chercherLocalement tdsenre x with            
                    | None ->
                    let nom = n^"."^x in
                    let info = InfoVar (nom,Undefined, 0, "") in
                    let ia = info_to_info_ast info in
                    ajouter tdsenre nom ia;
                    let t = analyse_tds_type tdsenre typage in
                    modifier_type_info t ia;
                    (t,ia)
                    | Some _ ->
                    raise (DoubleDeclaration x)
                    end
                    in
                    let list = List.map (ajouter_champs) lc in
                    let (nlc,ial) = List.split list in 
                    let info = InfoEnre (n,[],ial,0,0,"") in
                    let ia = info_to_info_ast info in
                      ajouter tdsbloc x ia; 
                      (RecordTds(nlc),ia)

          | _ -> 
            let info = InfoVar (x,Undefined, 0, "") in
            let ia = info_to_info_ast info in
            ajouter tdsbloc x ia;
            let typ = analyse_tds_type maintds typage in 
            (typ, ia)
          end
        | Some _ ->
          raise (DoubleDeclaration x)
        in 
          let lpn = List.map ajouter_para lp in
          let lin = List.map (analyse_tds_instruction tdsbloc) li in
          let typ = analyse_tds_type maintds t in 
          Fonction(typ,ia,lpn,lin)    
              
       

(* analyser : AstSyntax.ast -> AstTds.ast *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.ast *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (deftypes,fonctions,prog)) =
  let tds = creerTDSMere () in
  let _ = List.map (analyse_tds_deftype tds) deftypes in 
  let nf = List.map (analyse_tds_fonction tds) fonctions in 
  let nb = analyse_tds_bloc tds prog in
  Programme (nf,nb)

end
