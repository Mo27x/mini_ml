open Type_system
open Ast

exception Constraint_error of type_lang * type_lang
exception Typing_error of Util.Position.t * string

module Counter = struct
  type t = int ref

  let create () = ref 0

  let get_fresh counter =
    let res = !counter in
    counter := !counter + 1;
    res
end

let type_of_built_in (built_in : built_in) =
  match built_in with
  | Add | Sub | Mul | Div | Mod -> TFunc ([], TInt, TFunc ([], TInt, TInt))
  | And | Or -> TFunc ([], TBool, TFunc ([], TBool, TBool))
  | Eq | Neq | Lt | Gt | Leq | Geq ->
      TFunc ([ 0 ], TUniv 0, TFunc ([], TUniv 0, TBool))
  | Concat -> TFunc ([], TString, TFunc ([], TString, TString))
  | Cat | Append ->
      TFunc
        ([ 0 ], TList ([], TUniv 0), TFunc ([], TUniv 0, TList ([], TUniv 0)))
  | UMin -> TFunc ([], TInt, TInt)
  | Not -> TFunc ([], TBool, TBool)
  | Head -> TFunc ([ 0 ], TList ([], TUniv 0), TUniv 0)
  | Tail -> TFunc ([ 0 ], TList ([], TUniv 0), TList ([], TUniv 0))
  | Print -> TFunc ([ 0 ], TList ([], TUniv 0), TUnit)

let rec solve_constraints (constraints : (type_lang * type_lang) list) :
    (int * type_lang) list =
  match constraints with
  | [] -> []
  | cstraint :: rest -> (
      match cstraint with
      | TUniv t, TUniv n ->
          let mini, maxi = if t < n then (t, n) else (n, t) in
          solve_constraints
            (List.map (substitute_constraint maxi (TUniv mini)) rest)
      | (t, TUniv n | TUniv n, t) when t <> TUnit ->
          if List.mem n (get_free_type_var t) then
            raise (Constraint_error (t, TUniv n))
          else solve_constraints (List.map (substitute_constraint n t) rest)
      | TFunc (_, arg1, res1), TFunc (_, arg2, res2) ->
          solve_constraints ([ (arg1, arg2); (res1, res2) ] @ rest)
      | TList (_, t1), TList (_, t2) -> solve_constraints ([ (t1, t2) ] @ rest)
      | t1, t2 ->
          if t1 = t2 then solve_constraints rest
          else raise (Constraint_error (t1, t2)))

let instantiate (counter : Counter.t) (type_lang : type_lang) : type_lang =
  let instancied_generics =
    List.fold_left
      (fun acc original -> (original, TUniv (Counter.get_fresh counter)) :: acc)
      []
      (get_type_generic type_lang)
  in
  apply_subst_in_type instancied_generics type_lang
