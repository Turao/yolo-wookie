(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Types
NOT USING YET

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