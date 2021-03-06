/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token <string> TID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token EOF

%token NEW
%token NULL
%token ADR

%token PLUSEGAL

%token TYPEDEF

%token STRUCT
%token PT

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction list> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e 
%type <expression list> cp
%type <affectable> a
%type <typedef list> td

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| tdl= td lf = fonc  ID li = bloc   {Programme (tdl,lf,li)}


fonc : 
|                                             {[]}
|t=typ n=ID PO p=dp PF AO li=is AF lf = fonc  {Fonction(t,n,p,li)::lf}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)} 
| n=a EQUAL e1=e PV                 {Affectable (n,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| RETURN exp=e PV                   {Retour (exp)}
| aff=a PLUSEGAL expr=e PV          {AssignationPlus(aff,expr)}
| TYPEDEF n=TID EQUAL t=typ  PV     {DeclarationType (Typedef(n,t))}

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

typ :
| BOOL                {Bool}
| INT                 {Int}
| RAT                 {Rat}
| t=typ MULT          {Adr (t)}
| n=TID               {Tid(n)}
| STRUCT AO p=dp AF   {Record(p)}

e : 
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Binaire(Fraction,e1,e2)}
| TRUE                    {Booleen true}
| FALSE                   {Booleen false}
| e=ENTIER                {Entier e}
| NUM e1=e                {Unaire(Numerateur,e1)}
| DENOM e1=e              {Unaire(Denominateur,e1)}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO exp=e PF             {exp}
| aff=a                   {Affectation (aff)}
| NULL                    {Null}
| PO NEW t=typ PF         {New (t)}
| ADR n=ID                {Adr (n)}
| AO lp=cp AF             {Enre(lp)}

cp :
|               {[]}
| e1=e le=cp    {e1::le}

a :
| n=ID                    {Ident (n)}
| PO MULT aff=a PF        {Deref (aff)}
| PO aff=a PT n=ID PF     {Champ (aff,n)}

td : 
|                                                 {[]}
| TYPEDEF n=TID EQUAL t=typ  PV tidl=td           {Typedef(n,t)::tidl}