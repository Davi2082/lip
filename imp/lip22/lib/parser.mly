%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST

%token SKIP
%token BREAK
%token TAKES
%token SEQ
%token IF
%token THEN
%token ELSE
%token REPEAT
%token FOREVER

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EOF

%token PROC
%token VAL
%token REF
%token INT
%token ARRAY

%left SEQ
%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <prog> prog

%%

prog:
  | dv = decl_v; dp = decl_p; c = cmd; EOF { Prog(dv,dp,c) }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | x = ID { Var(x) }
  | x = ID; LBRACKET; e=expr; RBRACKET; { ArrayEl(x,e) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | SKIP { Skip }
  | BREAK { Break }
  | IF; e0 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | REPEAT; c = cmd; FOREVER; { Repeat(c) }
  | x = ID; TAKES; e=expr; { Assign(x,e) }
  | x = ID; LBRACKET; e1=expr; RBRACKET; TAKES; e2=expr; { Assign(ArrayEl(x,e1),e2) }
  | f = ID; LPAREN; pa=expr; RPAREN { Call(f,pa) }
  | c1 = cmd; SEQ; c2 = cmd; { CSeq(c1,c2) }
  | LBRACE; d = decl_v; SEQ; c = cmd; RBRACE; { Block(d,c) }
;

par_f:
  | VAL; x = ID; { Val(x) }
  | REF; x = ID; { Ref(x) }
;

decl_v:
  | INT; x = ID { IntVar(x) }
  | ARRAY; x = ID; LBRACKET; dim=expr; RBRACKET; { ArrayDecl(x,dim) }
  | d1 = decl_v; SEQ; d2=decl_v; { DVSeq(d1,d2) }
  | { NullVar }
;

decl_p:
  | PROC; p = ID; LPAREN; x = par_f; RPAREN; LBRACE; c = cmd; RBRACE { Proc(p,x,c) }
  | d1 = decl_p; SEQ; d2 = decl_p; { DPSeq(d1,d2) } 
  | { NullProc }
;