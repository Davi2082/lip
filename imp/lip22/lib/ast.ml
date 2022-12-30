type ide = string ;;

type par_f = Val of ide | Ref of ide ;;

type expr =
  | True
  | False
  | Var of ide
  | ArrayEl of ide * expr
  | Const of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
;;

type decl_v =
  | NullVar
  | ArrayDecl of ide * int
  | IntVar of ide
  | DVSeq of decl_v * decl_v
;;

type cmd =
  | Skip
  | Break
  | Assign of ide * expr
  | Assign_a of expr * expr
  | CSeq of cmd * cmd
  | Block of decl_v * cmd
  | If of expr * cmd * cmd
  | Repeat of cmd
  | Call of ide * expr    
  | CallExec of cmd * expr (* Runtime only: c is the cmd being reduced, e is the return expr *)
;;

type decl_p =                       
  | NullProc                      
  | Proc of ide * par_f * cmd   
  | DPSeq of decl_p * decl_p  
;;

type par_a = expr ;;

type prog = Prog of decl_p * decl_v * cmd ;;