(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Alunos:
	Arthur Lenz
	Rafael Valer *)

(* L1 Types *)
(* type types = T_Natural | T_Bool | T_Function of types * types *)

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
