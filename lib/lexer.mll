{
open Parser
exception SyntaxError of string
}

let var = ['a'-'z' 'A'-'Z' '0'-'9' '_']+
let white = [' ' '\t']+

rule read =
  parse
  | white    { read lexbuf }
  | "("      { LEFT_PAREN }
  | ")"      { RIGHT_PAREN }
  | "\\"     { LAMBDA }
  | "->"     { ARROW }
  | "let"    { LET }
  | "="      { EQUAL }
  | "in"     { IN }
  | var as v { VAR v }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
