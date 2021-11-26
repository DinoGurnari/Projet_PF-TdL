
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TRUE
    | SLASH
    | RETURN
    | RAT
    | PV
    | PRINT
    | PO
    | PLUS
    | PF
    | NUM
    | MULT
    | INT
    | INF
    | IF
    | ID of (
# 11 "parser.mly"
       (string)
# 26 "parser.ml"
  )
    | FALSE
    | EQUAL
    | EOF
    | ENTIER of (
# 10 "parser.mly"
       (int)
# 34 "parser.ml"
  )
    | ELSE
    | DENOM
    | CONST
    | CO
    | CF
    | CALL
    | BOOL
    | AO
    | AF
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState88
  | MenhirState82
  | MenhirState78
  | MenhirState76
  | MenhirState71
  | MenhirState66
  | MenhirState55
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState46
  | MenhirState43
  | MenhirState41
  | MenhirState38
  | MenhirState35
  | MenhirState32
  | MenhirState28
  | MenhirState22
  | MenhirState17
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState8
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 3 "parser.mly"
  

open Type
open Ast.AstSyntax

# 96 "parser.ml"

let rec _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.programme) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
            let _v : (Ast.AstSyntax.programme) = 
# 56 "parser.mly"
                          (lfi)
# 114 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.AstSyntax.programme)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (lf : (Ast.AstSyntax.fonction))), _, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
        let _v : (Ast.AstSyntax.programme) = 
# 59 "parser.mly"
                          (let (Programme (lf1,li))=lfi in (Programme (lf::lf1,li)))
# 133 "parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.AstSyntax.expression))), _, (le : (Ast.AstSyntax.expression list))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression list) = 
# 105 "parser.mly"
                (e1::le)
# 150 "parser.ml"
         in
        _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 165 "parser.ml"
            ))), _, (lp : (Ast.AstSyntax.expression list))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 89 "parser.mly"
                          (AppelFonction (n,lp))
# 170 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_is : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.bloc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
            let _v : (Ast.AstSyntax.bloc) = 
# 64 "parser.mly"
                          (li)
# 199 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState41 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.instruction) = 
# 76 "parser.mly"
                                    (TantQue (exp,li))
# 210 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState50 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | AO ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState52 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li1 : (Ast.AstSyntax.bloc))), _, (li2 : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.instruction) = 
# 75 "parser.mly"
                                    (Conditionnelle (exp,li1,li2))
# 242 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.programme) = 
# 60 "parser.mly"
                          (Programme ([],li))
# 252 "parser.ml"
                 in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i1 : (Ast.AstSyntax.instruction))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
        let _v : (Ast.AstSyntax.bloc) = 
# 68 "parser.mly"
                          (i1::li)
# 270 "parser.ml"
         in
        _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 285 "parser.ml"
            ))), _, (p : ((Type.typ * string) list))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
            let _v : (Ast.AstSyntax.fonction) = 
# 62 "parser.mly"
                                         (Fonction(t,n,p,li))
# 290 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | ID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | RAT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_i : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | CONST ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ID _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IF ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | PRINT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RETURN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | AF ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_goto_dp : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Type.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 360 "parser.ml"
        ))), _, (lp : ((Type.typ * string) list))) = _menhir_stack in
        let _v : ((Type.typ * string) list) = 
# 81 "parser.mly"
                          ((t,n)::lp)
# 365 "parser.ml"
         in
        _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | CONST ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | ID _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | IF ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | PRINT ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | RAT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | RETURN ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | WHILE ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | AF ->
                    _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_e : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | CO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | DENOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ENTIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | NUM ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PO ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PF ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 90 "parser.mly"
                          (Binaire(Fraction,e1,e2))
# 506 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression) = 
# 96 "parser.mly"
                          (Unaire(Denominateur,e1))
# 522 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression) = 
# 95 "parser.mly"
                          (Unaire(Numerateur,e1))
# 532 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | INF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | MULT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 101 "parser.mly"
                          (exp)
# 629 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 97 "parser.mly"
                          (Binaire (Plus,e1,e2))
# 678 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 98 "parser.mly"
                          (Binaire (Mult,e1,e2))
# 700 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 100 "parser.mly"
                          (Binaire (Inf,e1,e2))
# 722 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 99 "parser.mly"
                          (Binaire (Equ,e1,e2))
# 744 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 77 "parser.mly"
                                    (Retour (exp))
# 777 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 74 "parser.mly"
                                    (Affichage (e1))
# 799 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (
# 11 "parser.mly"
       (string)
# 831 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 72 "parser.mly"
                                    (Affectation (n,e1))
# 836 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 857 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 71 "parser.mly"
                                    (Declaration (t,n,e1))
# 862 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.expression list) = 
# 104 "parser.mly"
                ([])
# 879 "parser.ml"
     in
    _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.bloc) = 
# 67 "parser.mly"
                          ([])
# 888 "parser.ml"
     in
    _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1011 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | CO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | DENOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | ENTIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | FALSE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | ID _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | NUM ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | PO ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | TRUE ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ENTIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | PV ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 1082 "parser.ml"
                    ))), (e : (
# 10 "parser.mly"
       (int)
# 1086 "parser.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.AstSyntax.instruction) = 
# 73 "parser.mly"
                                    (Constante (n,e))
# 1091 "parser.ml"
                     in
                    _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Type.typ * string) list) = 
# 80 "parser.mly"
                          ([])
# 1129 "parser.ml"
     in
    _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 92 "parser.mly"
                          (Booleen true)
# 1140 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1205 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 11 "parser.mly"
       (string)
# 1213 "parser.ml"
    )) = _v in
    let _v : (Ast.AstSyntax.expression) = 
# 91 "parser.mly"
                          (Ident n)
# 1218 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 93 "parser.mly"
                          (Booleen false)
# 1229 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 1236 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (
# 10 "parser.mly"
       (int)
# 1244 "parser.ml"
    )) = _v in
    let _v : (Ast.AstSyntax.expression) = 
# 94 "parser.mly"
                          (Entier e)
# 1249 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | DENOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ENTIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | FALSE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | NUM ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PO ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TRUE ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | CO ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | DENOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ENTIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | FALSE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ID _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | NUM ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PO ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TRUE ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | PF ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | CONST ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ID _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | IF ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | PRINT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RETURN ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | WHILE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | AF ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState82 | MenhirState71 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | CO ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | DENOM ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | ENTIER _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | FALSE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | ID _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | NUM ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | PO ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | TRUE ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | RAT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | PF ->
                    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | RAT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PF ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 86 "parser.mly"
          (Rat)
# 1644 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 85 "parser.mly"
          (Int)
# 1655 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1662 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AO ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 84 "parser.mly"
          (Bool)
# 1683 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.AstSyntax.programme) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RAT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 1727 "parser.ml"
