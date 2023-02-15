open! Core
module LC = Lambda_calculus

type qname = string [@@deriving sexp, eq]

(* Types *)
type typ =
  | TVar of tv ref
  | QVar of qname
  | TArrow of typ * typ
[@@deriving sexp, eq]

and tv =
  | Unbound of string
  | Link of typ
[@@deriving sexp, eq]

type env = (LC.varname * typ) list
type exp = LC.exp

let rec pretty_typ = function
  | TVar { contents = tv } -> pretty_tv tv
  | QVar name -> name
  | TArrow (ty1, ty2) ->
    String.concat [ "("; pretty_typ ty1; " -> "; pretty_typ ty2; ")" ]

and pretty_tv = function
  | Unbound name -> "'" ^ name
  | Link ty -> pretty_typ ty
;;

let empty_env = []
let gensym_counter = ref 0
let reset_gensym () = gensym_counter := 0

let gensym () =
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26
  then String.make 1 (Char.of_int_exn (Char.to_int 'a' + n))
  else "t" ^ string_of_int n
;;

(* Make a fresh type variable *)
let newvar () = TVar (ref (Unbound (gensym ())))
let env_lookup env v = List.Assoc.find ~equal:String.equal env v

let env_lookup_exn env v =
  match env_lookup env v with
  | None -> failwith (String.concat [ "Variable "; v; " is not bound." ])
  | Some t -> t
;;

let env_typ_name_occurs env name =
  let typs = List.map ~f:snd env in
  let rec typ_contains_name = function
    | TVar { contents = Unbound name' } -> String.equal name name'
    | TVar { contents = Link ty } -> typ_contains_name ty
    | QVar _ -> false
    | TArrow (ty1, ty2) -> typ_contains_name ty1 || typ_contains_name ty2
  in
  List.find ~f:typ_contains_name typs |> Option.is_some
;;

let env_insert env v t = (v, t) :: env

(* Replace QVars with fresh TVars *)
let inst ty =
  let rec go subst = function
    | QVar name ->
      (match env_lookup subst name with
      | Some tv -> tv, subst
      | None ->
        let tv = newvar () in
        tv, env_insert subst name tv)
    | TVar { contents = Link ty } -> go subst ty
    | TVar { contents = Unbound _ } as ty -> ty, subst
    | TArrow (ty1, ty2) ->
      let ty1, subst = go subst ty1 in
      let ty2, subst = go subst ty2 in
      TArrow (ty1, ty2), subst
  in
  fst (go [] ty)
;;

let rec occurs_check tvref = function
  | TVar tvref' when equal_tv !tvref !tvref' -> failwith "Occurs check failed"
  | TVar { contents = Link ty } -> occurs_check tvref ty
  | TArrow (t1, t2) ->
    occurs_check tvref t1;
    occurs_check tvref t2
  | _ -> ()
;;

let rec unify t1 t2 =
  if equal_typ t1 t2
  then ()
  else (
    match t1, t2 with
    | TVar { contents = Link t1 }, t2 | t1, TVar { contents = Link t2 } -> unify t1 t2
    | TVar ({ contents = Unbound _ } as tvref), t'
    | t', TVar ({ contents = Unbound _ } as tvref) ->
      occurs_check tvref t';
      tvref := Link t'
    | TArrow (tyl1, tyl2), TArrow (tyr1, tyr2) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
    | _ -> failwith "Unification failed")
;;

let generalize env =
  let rec go = function
    | TVar { contents = Unbound name } as ty ->
      if env_typ_name_occurs env name then ty else QVar name
    | TVar { contents = Link ty } -> go ty
    | TArrow (ty1, ty2) -> TArrow (go ty1, go ty2)
    | ty -> ty
  in
  go
;;

let rec typeof env = function
  | LC.Var v -> inst (env_lookup_exn env v)
  | LC.App (e1, e2) ->
    let ty_fun = typeof env e1 in
    let ty_arg = typeof env e2 in
    let ty_result = newvar () in
    unify ty_fun (TArrow (ty_arg, ty_result));
    ty_result
  | LC.Lam (v, e) ->
    let ty_v = newvar () in
    let ty_e = typeof (env_insert env v ty_v) e in
    TArrow (ty_v, ty_e)
  | LC.Let (v, e, e2) ->
    let ty_e = typeof env e in
    typeof (env_insert env v (generalize env ty_e)) e2
;;
