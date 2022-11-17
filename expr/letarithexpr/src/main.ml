open Ast

exception TypeError of string;;
exception NoRuleApplies

let rec isnumericalv e = match e with
  Zero -> true
| Succ(e) -> isnumericalv(e)
| _ -> false
;;

let string_of_val n = string_of_int n ;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ (string_of_expr e) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "Zero"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec trace1 = function
    True -> Succ(Zero)
  | False -> Zero 
  | If(Succ(Zero),e1,_) -> e1
  | If(Zero,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(Succ(Zero)) -> Zero
  | Not(Zero) -> Succ(Zero)
  | Not(e) -> let e' = trace1 e in Not(e')

  | Or(Succ(e),_) when isnumericalv(e) -> True
  | Or(_,Succ(e)) when isnumericalv(e) -> True
  | Or(Zero,Zero) -> False
  | Or(Zero,e2) -> let e2' = trace1 e2 in Or(Zero,e2')
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)

  | And(Zero,_) -> False
  | And(_,Zero) -> False
  | And(Succ(e1),Succ(e2)) when(isnumericalv(e1) && isnumericalv(e2)) -> True
  | And(Succ(e1),e2) when(isnumericalv(e1)) -> let e2' = trace1 e2 in And(Succ(Zero),e2')
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)

  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> False
  | Pred(Succ(e)) -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when isnumericalv(e) -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval = function
    True -> 1
  | False -> 0

  | If(e0,e1,e2) -> if (eval e0) > 0 then (eval e1) else (eval e2)

  | Not(e) -> if (eval e) > 0 then 0 else 1

  | Or(e1,e2) -> if(eval e1 + eval e2) > 0 then 1 else 0

  | And(e1,e2) -> if(eval e1 * eval e2) > 0 then 1 else 0

  | Zero -> 0

  | Succ(e) -> (eval e) + 1

  | Pred(e) -> if (eval e) = 0 then 0 else ((eval e) - 1)

  | IsZero(e) -> if (eval e) = 0 then 1 else 0
;;