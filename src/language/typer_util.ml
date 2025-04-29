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
      TFunc ([], TUniv 0, TFunc ([], TUniv 0, TBool))
  | Concat -> TFunc ([], TString, TFunc ([], TString, TString))
  | Cat | Append ->
      TFunc ([], TList ([], TUniv 0), TFunc ([], TUniv 0, TList ([], TUniv 0)))
  | UMin -> TFunc ([], TInt, TInt)
  | Not -> TFunc ([], TBool, TBool)
  | Head -> TFunc ([], TList ([], TUniv 0), TUniv 0)
  | Tail -> TFunc ([], TList ([], TUniv 0), TList ([], TUniv 0))
  | Print -> TFunc ([], TList ([], TUniv 0), TUnit)

let rec solve_constraints (constraints : (type_lang * type_lang) list) :
    (int * type_lang) list =
  match constraints with
  (* à modifier après ça*)
  | [] -> failwith "solve_constraints not implemented"
  | ((TFunc _ as f), TBool) :: _ -> raise (Constraint_error (f, TBool))
  | _ :: t ->
      ignore substitute_constraint;
      solve_constraints t

let instantiate (counter : Counter.t) (type_lang : type_lang) : type_lang =
  (* à modifier*)
  let _ = Counter.get_fresh counter in
  match type_lang with _ -> type_lang
