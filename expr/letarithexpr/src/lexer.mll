{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | eof { EOF }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }