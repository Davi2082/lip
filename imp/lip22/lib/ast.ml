type ide = string
  
type expr =
  | True
  | False
  | Var of ide
  | Const of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Call of ide * expr    
  | CallExec of cmd * expr (* Runtime only: c is the cmd being reduced, e is the return expr *)
  | CallRet of expr        (* Runtime only: e is the return expr *)
              
and cmd =
  | Skip
  | Assign of string * expr
  | CSeq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd

type par_f = Val of ide | Ref of ide
type par_a = expr 

type decl_v =
  | NullVar
  | IntVar of ide
  | DVSeq of decl_v * decl_v
  | Array of ide * int

type decl_p =
  | NullProc
  | Proc of ide * pf * cmd
  | DPSeq of decl_p * decl_p

type prog = Prog of decl * cmd