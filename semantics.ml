(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Alunos:
	Arthur Lenz
	Rafael Valer *)

(* Types *)
type variable = string

type operator =
	Sum | Diff | Mult | Div | Eq | Leq


type expr =
			Num of int | Bool of bool | Bop of expr * operator * expr
			| If of expr * expr * expr | Var of variable
			| App of expr * expr
			| Lam of variable * expr | Let of variable * expr * expr
			| Lrec of variable * expr * expr


type value =
	| Vnum of int | Vbool of bool | Vclos of variable * expr * env
and
	env = ( variable * value ) list
(* ================================================================ *)



(* exceptions *)
(* exception CantEvaluateExpression of string;; *)
exception CantEvaluateIf of string;;



(* helpers *)
exception CantGetValueFromNone of string
let get (x: value option) : value =
	match x with
	| None -> raise (CantGetValueFromNone "Could not retrieve value from None") 
	| Some (y) -> y;;
(* ================================================================ *)


(* operators *)
let sum (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x)
		+
		(match b with
		| Vnum (y) -> y)
	);;

let diff (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x)
		-
		(match b with
		| Vnum (y) -> y)
	);;

let mult (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x)
		*
		(match b with
		| Vnum (y) -> y)
	);;
	
let div (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x)
		/
		(match b with
		| Vnum (y) -> y)
	);;
	
let eq (a: value) (b: value) : value =
	Vbool (
		(match a with
		| Vnum (x) -> x)
		==
		(match b with
		| Vnum (y) -> y)
	);;

let leq (a: value) (b: value) : value =
	Vbool (
		(match a with
		| Vnum (x) -> x)
		<=
		(match b with
		| Vnum (y) -> y)
	);;
(* ================================================================ *)


(* empties environment *)
let empty env : env = [ ]


(*
	busca no ambiente :
	
		lookup_env env x == Some v
		(x,v) é o par contendo x adicionando mais recentemente a env 
		lookup_env env x == None, se env não possui par com x 
*)
let rec lookup_env (env:env) (x:variable) : value option = 
	match env with
	| [] -> None
	| (var,value)  :: tl -> 
		if compare var x == 0 then 
			Some value 
		else 
			lookup_env tl x;; 


(* atualização do ambiente :
update env x v retorna um novo env contendo o par (x,v) *)
let update_env (env:env) (x:variable) (v: value) : env = 
	(x , v)::env 


(* Big step expression *)
let rec eval (env: env) (e: expr) : value option =
	match e with
	| Num e -> Some (Vnum e)
	| Bool e -> Some (Vbool e)
	| Var e -> lookup_env env e
	| If (e1, e2, e3) -> 
		( match eval env e1 with
		| Some (Vbool true) -> eval env e2
		| Some (Vbool false) -> eval env e3 
		| _ -> raise (CantEvaluateIf "Bad Syntax") )
	| Bop (e1, op, e2) ->
		(match op with
		| Sum ->  Some ( sum (get(eval env e1)) (get (eval env e2)) )
		| Diff ->  Some ( diff (get(eval env e1)) (get (eval env e2)) )
		| Mult ->  Some ( mult (get(eval env e1)) (get (eval env e2)) )
		| Div ->  Some ( div (get(eval env e1)) (get (eval env e2)) )
		| Eq ->  Some ( eq (get(eval env e1)) (get (eval env e2)) )
		| Leq ->  Some ( leq (get(eval env e1)) (get (eval env e2)) )
		| _ -> None;
		)
	| _ -> None;;



(*---------------------TESTS----------------------*)


let x : value = Vnum 10;;
let y : value = Vnum 12;;
let z : value = Vnum 9;;
let w : value = Vbool true;;
let k : value = Vbool true;;

let environment : env = [ ("y", y);("x", x);("z", z); ("w", w) ];;
let env1 = update_env environment "karakarambakarakarao" k

let result = lookup_env environment "w";;

env1;;



let exp0 : expr = Num 10;;
let exp1 : expr = Num 3;;
let exp2 : expr = Var "y";;

let exp3 : expr = If(Bool true, Bool false, Bool true);;
let exp4 : expr = If(exp0, Num 10, Num 12);;
let exp5 : expr = Bop (exp0, Sum, exp1);;

let bigstep : value option = eval environment exp5;;