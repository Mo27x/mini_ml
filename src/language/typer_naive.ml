open Ast
open Typer_util

let subenv (env : 'a Util.Environment.t) (added_vals : (string * 'a) list) =
  let new_env = Util.Environment.copy env in
  List.iter (fun (id, t) -> Util.Environment.add new_env id t) added_vals;
  new_env

let rec type_expr (counter : Counter.t) (env : type_lang Util.Environment.t)
    (expr : expr) =
  (* la suite est à modifier -- c’est juste là pour ne pas avoir de warning tant que vous ne travaillez pas dessus.*)
  match expr with
  | Cst_i (_,annot) -> 
    Annotation.set_type annot TInt;
    (TInt, [])
  | Cst_b (_,annot) -> 
    Annotation.set_type annot TBool;
    (TBool, [])
  | Cst_str (_,annot) -> 
    Annotation.set_type annot TString;
    (TString, [])
  | Cst_func (f, annot) -> 
    Annotation.set_type annot (type_of_built_in f);
    (type_of_built_in f, [])
  | Nil annot -> 
    let t = TList ([], TUniv (Counter.get_fresh counter)) in
    Annotation.set_type annot t;
    (t, [])
  | Unit annot -> 
    Annotation.set_type annot TUnit;
    (TUnit, [])
  | Var (id, annot) -> (
      match Util.Environment.get env id with
      | Some t -> 
        Annotation.set_type annot t;
        (t, [])
      | None -> failwith ("Undefined var id: " ^ id))
  | IfThenElse (e1, e2, e3, annot) ->
      let t1, constraints1 = type_expr counter env e1
      and t2, constraints2 = type_expr counter env e2
      and t3, constraints3 = type_expr counter env e3 in
      let newContraints =
        [ (t1, TBool); (t2, t3) ] @ constraints1 @ constraints2 @ constraints3
      in
      Annotation.set_type annot t2;
      (t2, newContraints)
  | App (efunc, earg, annot) ->
      let tfunc, cfunc = type_expr counter env efunc
      and targ, carg = type_expr counter env earg
      and tUnivA = TUniv (Counter.get_fresh counter)
      and tUnivB = TUniv (Counter.get_fresh counter) in
      let newContraints =
        [ (tfunc, TFunc ([], tUnivA, tUnivB)); (targ, tUnivB) ] @ cfunc @ carg
      in
      Annotation.set_type annot tUnivB;
      (tUnivB, newContraints)
  | Let (isRec, id, e1, e2, annot) ->
      if isRec then (
        let tUnivA = TUniv (Counter.get_fresh counter) in
        let sub_env1 = subenv env [ (id, tUnivA) ] in
        let t1, constraints1 = type_expr counter sub_env1 e1 in
        let sub_env2 = subenv env [ (id, t1) ] in
        let t2, constraints2 = type_expr counter sub_env2 e2 in
        Util.Environment.remove env id;
        Annotation.set_type annot t2;
        (t2, [ (tUnivA, t1) ] @ constraints1 @ constraints2))
      else
        let t1, constraints1 = type_expr counter env e1 in
        Util.Environment.add env id t1;
        let t2, constraints2 = type_expr counter env e2 in
        Util.Environment.remove env id;
        Annotation.set_type annot t2;
        (t2, constraints1 @ constraints2)
  | Fun (id, body, annot) ->
      let tUnivA = TUniv (Counter.get_fresh counter) in
      let body_env = subenv env [ (id, tUnivA) ] in
      let tbody, constraintsBody = type_expr counter body_env body in
      Util.Environment.add env id tbody;
      let t = TFunc ([], tUnivA, tbody) in
      Annotation.set_type annot t;
      (t, constraintsBody)
  | Ignore (e1, e2, annot) ->
      let _, constraints1 = type_expr counter env e1
      and t2, constraints2 = type_expr counter env e2 in
        Annotation.set_type annot t2;
        (t2, constraints1 @ constraints2)
