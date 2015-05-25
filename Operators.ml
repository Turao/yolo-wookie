(* Semântica Formal - Interpretador de L1 baseado em semântica
	big step com ambientes

Binary Operators
NOT USING YET

Alunos:
	Arthur Lenz
	Rafael Valer *)


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