(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Alunos:
	Arthur Lenz
	Rafael Valer *)

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


(* ambiente vazio *)
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




(*---------------------TESTS----------------------*)	


let x : value = Vnum 10;;
let y : value = Vnum 12;;
let z : value = Vnum 9;;
let k : value = Vbool true;;

let environment : env = [ ("y", y);("x", x);("z", z) ];;
let env1 = update_env environment "karakarambakarakarao" k

let result = lookup_env environment "z";;

env1;;



(*
let rec eval (env: env) (e: expr) : value option = failwith ”unimplemented”*)