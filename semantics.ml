(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Alunos:
	Arthur Lenz
	Rafael Valer *)

(* L1 Types *)
(* type types = T_Natural | T_Bool | T_Function of types * types *)

(*
type term = 
					(* L0 Abstract Grammar *)
						TmTrue
					| TmFalse
					| TmIf of term * term * term
					| TmZero
					| TmSucc
					| TmPred
					| TmIsZero
					(* extending to L1 Abstract Grammar *)
					(*| e1 op e2*) 
					| TmApp of term * term
					| TmVar of string
					| TmFun of string * term * term
					| TmLet of term * term * term

type value = 
						(* L1 values *)
						  ValBool
						| ValNatural
						| ValClosure
					*)


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
	[] -> None
	| (var,value)  :: tl -> print_endline (var) 


(*
let update env (env:env) (x:variable) (v: value) : env = failwith ”unimplemented”
let rec eval (env: env) (e: expr) : value option = failwith ”unimplemented”
*)


