%{
  open Lambda_calculus
%}
%token <string> VAR
%token LEFT_PAREN
%token RIGHT_PAREN
%token LAMBDA
%token ARROW
%token LET
%token EQUAL
%token IN
%token EOF

%start <exp> prog
%%

prog:
  | e = exp; EOF {e};

exp:
  | abs_exp { $1 }
  | app_exp { $1 }
  | let_exp { $1 }

abs_exp:
  | LAMBDA VAR ARROW exp { Lam ($2, $4) }

let_exp:
  | LET VAR EQUAL exp IN exp { Let ($2, $4, $6) }

app_exp:
  | app_exp app_exp_end { App ($1, $2) }
  | app_exp_end { $1 }

app_exp_end:
  | VAR { Var $1 }
  | LEFT_PAREN exp RIGHT_PAREN { $2 }
