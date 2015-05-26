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

(* usado para que devolva o valor referente a um valor opcional, podendo agora ser calculado pelas
operações binarias *)
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


let rec update_value (env:env) (x:variable) (v:value) : env =
	match env with
	| [] -> []
	| (var, value) :: tl -> 
		if compare var x == 0 then
			(var, v)::tl
		else
			(var, value) :: (update_value tl x v);;


(* atualização do ambiente :
update env x v retorna um novo env contendo o par (x,v) *)
let update_env (env:env) (x:variable) (v: value) : env = 
	match lookup_env env x with
	| None -> (x, v)::env
	| Some value -> 
	(* if the variable is found in the environment, checks if
	there is a need to change its value *)
		if value != v then 
			(update_value env x v)
		else
			env;; 
(* ================================================================ *)	



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
	(* evaluates both expressions and then operates*)
		let v1 = get(eval env e1) in
		let v2 = get(eval env e2) in
		(match op with
			| Sum ->  Some ( sum v1 v2 )
			| Diff ->  Some ( diff v1 v2 )
			| Mult ->  Some ( mult v1 v2 )
			| Div ->  Some ( div v1 v2 )
			| Eq ->  Some ( eq v1 v2 )
			| Leq ->  Some ( leq v1 v2 )
			| _ -> None;
		)
	| Lam (var, e1) ->
	(* se pa precisa verificar se a variavel eh valida aqui, ou seja, se existe no env *)
		Some (Vclos (var, e1, env))


	| App (e1, e2) -> 
		(* updates e1's environment with the value in e2 *)
		let updated_env : env =
			(* updated environment *)
			(update_env
				(* environment *)
				(match eval env e1 with
				| Some (Vclos (var, exp, env')) -> env' )
				(* variable *)
				(match eval env e1 with
				| Some (Vclos (var, exp, env)) -> var )
				(* value' *)
				(match eval env e2 with
				| Some v -> v))
		in
		(* and evaluates the expression e1 *)
		(match e1 with
		| Lam (var, exp) -> eval updated_env exp
		| _ -> eval updated_env e1)

		(* just some code, do not use it until things
		start to go wrong *)

	(* lookup_env
		(* updated environment *)
		(update_env
			(* environment *)
			(match eval env e1 with
			| Some (Vclos (var, exp, env')) -> env' )
			(* variable *)
			(match eval env e1 with
			| Some (Vclos (var, exp, env)) -> var )
			(* value' *)
			(match eval env e2 with
			| Some v -> v))
		(* variable *)
		(match eval env e1 with
		| Some (Vclos (var, exp, env)) -> var ) *)
	| _ -> None;;

(* ================================================================ *)



(*---------------------TESTS----------------------*)

(* values *)
let x : value = Vnum 10;;
let y : value = Vnum 12;;
let z : value = Vnum 9;;
let w : value = Vbool true;;
let k : value = Vbool true;;


let environment : env = [ ("y", y);("x", x);("z", z); ("w", w) ];;
let env1 = update_env environment "karakarambakarakarao" k

let result = lookup_env environment "w";;


(* expressions *)
let exp0 : expr = Num 10;;
let exp1 : expr = Num 3;;
let exp2 : expr = Var "y";;

let exp3 : expr = If(Bool true, Bool false, Bool true);;
let exp4 : expr = If(exp0, Num 10, Num 12);;
let exp5 : expr = Bop (exp0, Leq, exp1);;
let exp6 : expr = Lam ("w", exp0);;
let exp7 : expr = Bop (Var "y", Sum, Var "z")
let exp8 : expr = Lam ("z", exp7)
let exp9 : expr = App (exp8, exp0);;

(* environments *)
let env2 = update_env environment "y" (Vbool false);;
let env3 = update_env environment "y" (Vnum 999);;
let env4 = update_env environment "y" (Vnum 999);;
let env5 = update_env environment "z" (Vnum 177);;

(* big step evaluation *)
let bigstep : value option = eval environment exp9;;