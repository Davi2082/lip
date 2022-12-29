open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false

let rec trace1_expr st = function
  | Var x -> (Const(apply st x), st)

  | Not(True) -> (False,st)
  | Not(False) -> (True,st)
  | Not(e) -> let (e',st') = trace1_expr st e in (Not(e'),st')

  | And(True,e) -> (e,st)
  | And(False,_) -> (False,st)
  | And(e1,e2) -> let (e1',st') = trace1_expr st e1 in (And(e1',e2),st')

  | Or(True,_) -> (True,st)
  | Or(False,e) -> (e,st)
  | Or(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1',e2),st')

  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),st)
  | Add(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Add(Const(n1),e2'),st')
  | Add(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2),st')

  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),st)
  | Sub(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Sub(Const(n1),e2'),st')
  | Sub(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2),st')

  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),st)
  | Mul(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Mul(Const(n1),e2'),st')
  | Mul(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2),st')

  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,st) else (False,st)
  | Eq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Eq(Const(n1),e2'),st')
  | Eq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Eq(e1',e2),st')

  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,st) else (False,st)
  | Leq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Leq(Const(n1),e2'),st')
  | Leq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Leq(e1',e2),st')

  | Call(f,Const(n)) -> (match (topenv st) f with
        IFun(x,c,er) ->
        let l = getloc st in
        let env' = bind (topenv st) x (IVar l) in
        let mem' = bind (getmem st) l n in
        let st' = (env'::(getenv st), mem', l+1) in
        (CallExec(c,er),st')
      | _ -> raise (TypeError "Call of a non-function"))

  | Call(f,e) -> let (e',st') = trace1_expr st e in (Call(f,e'),st')

  | CallExec(c,e) -> (match trace1_cmd (Cmd(c,st)) with
      St st' -> (CallRet(e),st')
    | Cmd(c',st') -> (CallExec(c',e),st'))

  | CallRet(Const(n)) -> let st' = (popenv st, getmem st, getloc st) in (Const(n),st')

  | CallRet(e) -> let (e',st') = trace1_expr st e in (CallRet(e'),st')
  
  | _ -> raise NoRuleApplies

and trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,Const(n)) -> (match topenv st x with
        IVar l -> St (getenv st, bind (getmem st) l n, getloc st)
      | _ -> failwith "improve err msg")
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st') 
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')
    | While(e,c) -> Cmd(If(e,Seq(c,While(e,c)),Skip),st)

let rec sem_decl (e,l) = function
    EmptyDecl -> (e,l)
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Fun(f,x,c,er) -> let e' = bind e f (IFun(x,c,er)) in (e',l)
  | DSeq(d1,d2) -> let (e',l') = sem_decl (e,l) d1 in sem_decl (e',l') d2


let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

let trace n (Prog(d,c)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))


























  (* scritto da noi *)

let fst (x, _) = x

let rec eval_expr (e:expr) (env:env) (mem:mem) : int = 
  match e with
    True -> 1
  | False -> 0
  | Var x -> let IVar l = lookup x env in mem l
  | ArrayEl (x, i) -> let IVar l = lookup x env in 
                      let dim = fst (mem l) in (* com'è possibile che cerchiamo una coppia in un intero? *)
                      let index = eval_expr i env mem in
                      if index >= dim 
                        then failwith "Array index out of bounds" else mem (l + index + 1)
  | Const n -> n
  | Not e -> let n = eval_expr e env mem in if n = 0 then 1 else 0
  | And (e1, e2) -> let n1 = eval_expr e1 env mem in if n1 = 0 then 0 else eval_expr e2 env mem
  | Or (e1, e2) -> let n1 = eval_expr e1 env mem in if n1 <> 0 then 1 else eval_expr e2 env mem
  | Add (e1, e2) -> eval_expr e1 env mem + eval_expr e2 env mem
  | Sub (e1, e2) -> eval_expr e1 env mem - eval_expr e2 env mem
  | Mul (e1, e2) -> eval_expr e1 env mem * eval_expr e2 env mem
  | Eq (e1, e2) -> if eval_expr e1 env mem = eval_expr e2 env mem then 1 else 0
  | Leq (e1, e2) -> if eval_expr e1 env mem <= eval_expr e2 env mem then 1 else 0
  | Call _ -> failwith "Cannot evaluate a function call in eval_expr"
  | CallExec _ -> failwith "Cannot evaluate a function execution in eval_expr"


let rec eval_cmd (c:cmd) (env:env) (mem:mem) : env * mem = 
  match c with
    Skip -> (env, mem)
  | Break -> raise Break
  | Assign(x, e) -> (update_env env x (IVar(eval_expr e env mem)), mem)
  | Assign_a(ArrayEl(x, i), e) -> let loc = eval_expr (ArrayEl(x, i)) env mem 
                                  in (env, update_mem mem loc (eval_expr e env mem))
  | CSeq(c1, c2) -> let (env', mem') = eval_cmd c1 env mem 
                    in eval_cmd c2 env' mem'
  | Block(dv, c) -> let env' = eval_decl_v dv env 
                    in eval_cmd c env' mem
  | If(e, c1, c2) -> if eval_expr e env mem = 1 
                      then eval_cmd c1 env mem else eval_cmd c2 env mem
  | Repeat c -> try let (env', mem') = eval_cmd c env mem 
                    in eval_cmd (Repeat c) env' mem' with Break -> (env, mem)


let rec eval_par_f (pf:par_f) (env:env) (mem:mem) (pa:par_a) : env = 
  match pf with
    Val ide -> let loc = ref_loc env ide in update_mem loc pa mem;env
  | Ref ide -> let loc = ref_loc env ide in update_mem loc (eval_expr pa env mem) mem;env


let rec eval_par_a (p: par_a) (env: env) (mem: mem) : int = 
  match p with
  (* Caso 1: il parametro è una costante *)
  | Const c -> c
  (* Caso 2: il parametro è una variabile *)
  | Var v -> (try Env.find v env with Not_found -> Mem.find v mem)
  (* Caso 3: il parametro è un elemento di array *)
  | ArrayEl (a, e) ->
      let idx = eval_expr e env mem in
      let arr = (try Env.find a env with Not_found -> Mem.find a mem) in
      Array.get arr idx
  (* Caso 4: il parametro è un'espressione *)
  | _ as e -> eval_expr e env mem


let rec eval_decl_v (d: decl_v) (env: env) (mem: mem) : env * mem =
  match d with
  | NullVar -> (env, mem)
  | IntVar id -> (Env.add id (ref 0) env, mem)
  | DVSeq (d1, d2) ->
      let (env', mem') = eval_decl_v d1 env mem in
      eval_decl_v d2 env' mem'
  | ArrayDecl (id, size) ->
      let a = Array.make size 0 in
      (Env.add id (ref a) env, Mem.add id a mem)


let rec eval_decl_p (dp: decl_p) (env: env) (p_env: p_env) : p_env =
  match dp with
  | NullProc -> p_env
  | Proc (id, pf, c) ->
    (* Aggiungiamo la procedura all'ambiente di procedura *)
    let p_env' = StringMap.add id (pf, c) p_env in
    p_env'
  | DPSeq (dp1, dp2) ->
    (* Eval_decl_p per ogni dichiarazione di procedura nella sequenza *)
    let p_env' = eval_decl_p dp1 env p_env in
    eval_decl_p dp2 env p_env'


let rec eval_prog (p:prog) (env:env) (mem:mem) : mem =
  match p with
  | Prog (dp, dv, c) ->
      let env' = eval_decl_p dp env in
      let (env'', mem') = eval_decl_v dv env' mem in
      eval_cmd c env'' mem'
