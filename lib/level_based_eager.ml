open! Core
module LC = Lambda_calculus

let inference_strategy = "Level-based eager"

type qname = string [@@deriving sexp, eq]
type level = int [@@deriving sexp, eq]

(* Types *)
type typ =
  | TVar of tv ref
  | QVar of qname
  | TArrow of typ * typ
[@@deriving sexp, eq]

and tv =
  | Unbound of string * level
  | Link of typ
[@@deriving sexp, eq]

let rec typ_is_arrow = function
  | TArrow _ -> true
  | TVar { contents = Link ty } -> typ_is_arrow ty
  | _ -> false
;;

let rec typ_links_resolved = function
  | TVar { contents = Link ty } -> typ_links_resolved ty
  | TArrow (ty1, ty2) -> TArrow (typ_links_resolved ty1, typ_links_resolved ty2)
  | ty -> ty
;;

type env = (LC.varname * typ) list
type exp = LC.exp [@@deriving sexp, eq]

let rec pretty_typ = function
  | TVar { contents = tv } -> pretty_tv tv
  | QVar name -> name
  | TArrow (ty1, ty2) ->
    let pretty_ty1_raw = pretty_typ ty1 in
    let pretty_ty1 =
      if typ_is_arrow ty1
      then String.concat [ "("; pretty_ty1_raw; ")" ]
      else pretty_ty1_raw
    in
    let pretty_ty2 = pretty_typ ty2 in
    String.concat [ pretty_ty1; " -> "; pretty_ty2 ]

and pretty_tv = function
  | Unbound (name, _) -> "'" ^ name
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
let newvar current_level () = TVar (ref (Unbound (gensym (), current_level)))
let env_lookup env v = List.Assoc.find ~equal:String.equal env v

let env_lookup_exn env v =
  match env_lookup env v with
  | None -> failwith (String.concat [ "Variable "; v; " is not bound." ])
  | Some t -> t
;;

let env_insert env v t = (v, t) :: env

(* Replace QVars with fresh TVars *)
let inst current_level ty =
  let rec go subst = function
    | QVar name ->
      (match env_lookup subst name with
      | Some tv -> tv, subst
      | None ->
        let tv = newvar current_level () in
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

let rec occurs_check_with_level_update tvref = function
  | TVar tvref' when equal_ref equal_tv tvref tvref' ->
    failwith ("Occurs check failed: " ^ pretty_tv !tvref)
  | TVar ({ contents = Unbound (name, l') } as tvref') ->
    let min_level =
      match !tvref with
      | Unbound (_, l) -> min l l'
      | _ -> l'
    in
    tvref' := Unbound (name, min_level)
  | TVar { contents = Link ty } -> occurs_check_with_level_update tvref ty
  | TArrow (t1, t2) ->
    occurs_check_with_level_update tvref t1;
    occurs_check_with_level_update tvref t2
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
      occurs_check_with_level_update tvref t';
      tvref := Link t'
    | TArrow (tyl1, tyl2), TArrow (tyr1, tyr2) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
    | _ -> failwith "Unification failed")
;;

let generalize current_level =
  let rec go = function
    | TVar { contents = Unbound (name, l) } when l > current_level -> QVar name
    | TVar { contents = Link ty } -> go ty
    | TArrow (ty1, ty2) -> TArrow (go ty1, go ty2)
    | ty -> ty
  in
  go
;;

let typeof env exp =
  let rec go current_level env = function
    | LC.Var v -> inst current_level (env_lookup_exn env v)
    | LC.App (e1, e2) ->
      let ty_fun = go current_level env e1 in
      let ty_arg = go current_level env e2 in
      let ty_result = newvar current_level () in
      unify ty_fun (TArrow (ty_arg, ty_result));
      ty_result
    | LC.Lam (v, e) ->
      let ty_v = newvar current_level () in
      let ty_e = go current_level (env_insert env v ty_v) e in
      TArrow (ty_v, ty_e)
    | LC.Let (v, e, e2) ->
      let ty_e = go (current_level + 1) env e in
      let generalized_ty_e = generalize current_level ty_e in
      go current_level (env_insert env v generalized_ty_e) e2
  in
  reset_gensym ();
  typ_links_resolved (go 1 env exp)
;;
