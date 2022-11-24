{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = letter chr*
let nr = ['0'-'9']
let const = nr*

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }

  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }

  | "=" { EQ }
  | "<=" { LEQ }

  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }

  | "while" { WHILE }
  | "do" { DO }

  | "(" { LPAREN }
  | ")" { RPAREN }
  
  | eof { EOF }

  | id { VAR (Lexing.lexeme lexbuf) }
  | const { CONST (int_of_string(Lexing.lexeme lexbuf)) }