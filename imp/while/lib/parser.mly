%{
open Ast
%}

%token ID

%token TRUE
%token FALSE

%token VAR
%token CONST

%token NOT
%token AND
%token OR

%token ADD
%token SUB
%token MUL

%token EQ
%token LEQ

%token SKIP
%token ASSIGN
%token SEQ

%token IF
%token THEN
%token ELSE

%token WHILE
%token DO

%token LPAREN
%token RPAREN

%token EOF

%nonassoc ELSE
%left OR
%left AND
%left NOT

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }

  | x = VAR; { Var(x) }
  | n = CONST; { Const(n) }

  | NOT; e = expr; { Not (e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }

  | e1 = expr; ADD; e2 = expr; { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1, e2) }

  | e1 = expr; EQ; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1, e2) }

  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | SKIP; { Skip }
  | x = VAR; ASSIGN; e = CONST; { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1, c2) }

  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | WHILE; e = expr; DO; c = cmd; { While(e, c) }

  | LPAREN; c = cmd; RPAREN { c }
;;