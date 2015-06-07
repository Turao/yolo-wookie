
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
			| Lrec of variable * variable * expr * expr


type value =
	| Vnum of int | Vbool of bool | Vclos of variable * expr * env | Vrclos of variable * variable * expr * env
and
	env = ( variable * value ) list
(* ================================================================ *)



(* exceptions *)
(* exception CantEvaluateExpression of string;; *)
exception CantEvaluateIf of string;;



(* helpers *)
exception CantGetValueFromNone of string ;;

exception CantGetValue of string;;
exception CantEvaluate of string;;

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
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		+
		(match b with
		| Vnum (y) -> y
		| _ -> raise (CantGetValue "Cant get value") )
	);;

let diff (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		-
		(match b with
		| Vnum (y) -> y
		| _ -> raise (CantGetValue "Cant get value") )
	);;

let mult (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		*
		(match b with
		| Vnum (y) -> y
		| _ -> raise (CantGetValue "Cant get value") )
	);;
	
let div (a: value) (b: value) : value =
	Vnum (
		(match a with
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		/
		(match b with
		| Vnum (y) -> y
		| _ -> raise (CantGetValue "Cant get value") )
	);;
	
let eq (a: value) (b: value) : value =
	Vbool (
		(match a with
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		==
		(match b with
		| Vnum (y) -> y
		| _ -> raise (CantGetValue "Cant get value") )
	);;

let leq (a: value) (b: value) : value =
	Vbool (
		(match a with
		| Vnum (x) -> x
		| _ -> raise (CantGetValue "Cant get value") )
		<=
		(match b with
		| Vnum (y) -> y  
		| _ -> raise (CantGetValue "Cant get value") )
	);;
(* ================================================================ *)


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

(* remove the first occurence of a variable from an environment *)
let rec removeVariableFromEnvironment (env:env) (x:variable) : env = 
	match env with
	| [] -> []
	| (var, value) :: tl ->
		if compare var x == 0 then 
			tl
		else
			(var, value) :: (removeVariableFromEnvironment tl x);; 



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
(*
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
*)

let update_env (env:env) (x:variable) (v: value) : env =
	(x,v)::env

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
			| Leq ->  Some ( leq v1 v2 );
		)

	| Lam (var, e1) ->
		Some (Vclos (var, e1, env))

	
    | App (e1, e2) ->
    	let e1'= eval env e1 in
    	(match e1' with
    	| Some ( Vclos (var, exp, env')) -> 
    		(let e2' = eval env e2 in
    			( match e2' with
    				| Some v -> 
    					let updated_env : env = (update_env env' var v) in
    					eval updated_env exp
    				| _ -> raise (CantEvaluate "Cant evaluate")
    			)
    		)
    	| Some (Vrclos(f, x, e, env')) -> 
    		(let e2' = eval env e2 in
    			( match e2' with 
    				| Some v ->
    					let updated_env : env = (update_env (update_env env' x v) f (Vrclos(f, x, e, env'))) in
    					eval updated_env e
    				| _ -> raise (CantEvaluate "Cant evaluate")
    			)
    		)
    	| _ -> None
    	)

	| Let (var, e1, e2) -> 
		let updated_env : env =
			(update_env
				(* environment *)
				env
				(* variable *)
				var
				(* value' *)
				(match eval env e1 with
				| Some v -> v
				| _ -> raise (CantEvaluate "Cant evaluate")
				)
			)
		(* and evaluates the expression e2 *)
		in eval updated_env e2
	
	| Lrec (f, x, e1, e2) -> let closure = Vrclos(f, x, e1, env) in
          (eval (update_env env f closure) e2)

	;;

(* ============================================================================================================= *)


(* empties environment *)
let empty env : env = [ ];;

(*---------------------TESTS----------------------*)

(* ------------ environment tests ------------------*)

print_endline("--------- ENVIRONMENT TESTS --------");;

let test_env1 : env = [("var1", Vnum 1)];;
let test_env2 : env = update_env test_env1 "var2" (Vnum 2) ;;
let test_env3 : env = update_env test_env2 "var3" (Vbool true) ;;
let test_env4 : env = update_env test_env3 "var4" (Vbool false) ;;
let test_env5 : env = update_env test_env4 "var5" (Vnum 5) ;;

lookup_env test_env5 "var2" ;;
lookup_env test_env5 "var4" ;;

(* ------------- Expressions and Variables ---------- *)

print_endline("--------- EXPRESSIONS AND VALUES --------");;

let a : value = Vnum 10;;
let b : value = Vnum 12;;
let c : value = Vnum 9;;
let d : value = Vbool true;;
let e : value = Vbool false;;
let f : value = Vnum 1;;

let expr1 : expr = Num 10;;
let expr2 : expr = Num 20;;

(* ------------ Var tests ------------------------ *)

print_endline("--------- VAR TESTS --------");;

let var1 : expr = Var "var1";;
let varTrue : expr = Var "var3";;
let varFalse : expr = Var "var4";;
eval test_env5 var1;;
eval test_env5 varTrue;;
eval test_env5 varFalse;;

(* ------------ Bop tests ------------------------ *)

print_endline("--------- BOP TESTS --------");;

let test_Bop1 : expr = Bop(expr1, Sum, expr2);;
let test_Bop2 : expr = Bop(expr1, Diff, expr2);;
let test_Bop3 : expr = Bop(expr2, Div, expr1);;
let test_Bop4 : expr = Bop(expr1, Mult, expr2);;
let test_Bop5 : expr = Bop(expr1, Eq, expr2);;
let test_Bop6 : expr = Bop(expr1, Leq, expr2);;
eval test_env5 test_Bop1;;
eval test_env5 test_Bop2;;
eval test_env5 test_Bop3;;
eval test_env5 test_Bop4;;
eval test_env5 test_Bop5;;
eval test_env5 test_Bop6;;

(* ------------ If tests ------------------------ *)

print_endline("--------- IF TESTS --------");;

let test_If1 : expr = If(varTrue, test_Bop1, test_Bop2);;
let test_If2 : expr = If(varFalse, test_Bop1, test_Bop2);;
eval test_env5 test_If1;;
eval test_env5 test_If2;;

(* ------------ Lam tests ------------------------ *)

print_endline("--------- LAM TESTS --------");;

let envWithClosures : env = [ ("a", a);("b", b);("c", c); ("d", d); ("e", e); ("f", f)   ];;

let expression1 : expr = Bop (Var "a", Sum, Var "b");;
let expression2 : expr = If (Var "e", Var "a", Bop (Var "a", Diff, Var "b") );;
let test_Lam1 : expr = Lam ("z", expression1) ;;
let test_Lam2 : expr = Lam ("k", expression2) ;;
eval envWithClosures test_Lam1 ;;
eval envWithClosures test_Lam2 ;;

(* ------------ Let tests ------------------------ *)

print_endline("--------- LET TESTS --------");;

let exprLET1 : expr = Bop (Num 31, Sum, Num 33);;
let exprLET2 : expr = Bop (Var "f", Div, Num 2);;
let exprLET3 : expr = Let ("f", exprLET1, exprLET2);;

eval envWithClosures exprLET3;;

let exprLET4 : expr = Num 5;;
let exprLET5 : expr = Bop (Var "g", Leq, Num 10);;
let exprLET6 : expr = Let ("g", exprLET4, exprLET5);;

eval envWithClosures exprLET6;;

(* ------------ App tests ------------------------ *)

print_endline("--------- APP TESTS --------");;

let exprAPP1 : expr = Bop (Var "f", Sum, Num 10);;
let exprAPP2 : expr = Num 6;; 
let exprAPP3 : expr = Lam ("f", exprAPP1);;
let exprAPP4 : expr = App (exprAPP3, exprAPP2);;

eval envWithClosures exprAPP4;;


(* ------------ Lrec tests ------------------------ *)

print_endline("--------- LREC TESTS --------");;

let recTextBop = Bop(Var("x"), Eq, Num(1));;
let recTestTrue = Num(1);;
let recTextFalse = Bop(Var("x"), Mult,App(Var("f"), Bop(Var("x"), Diff, Num(1))));; (* x*f(x-1) *)
let varFunc = Var("f");;

let rec fnrec = (Lrec("f", "x", If(recTextBop, recTestTrue, recTextFalse), varFunc));;
let fat5 = App (fnrec, Num 5);;
let lrec_env : env = [];; (* ("x", Vnum 1) *)
eval lrec_env fat5;;






