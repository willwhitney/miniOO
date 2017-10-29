(* File calculatorLEX.mll *)
{
open CalculatorMENHIR;; (* Type token defined in CalculatorMENHIR.mli *)
exception Eof;;
}
rule token = parse
    [' ' '\t'] { token lexbuf } (* skip blanks and tabs *)
  | ['\n' ]    { EOL }
  | (['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as var
               { VAR var }
  | "var"      {VARDEC}
  | (['a'-'z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])* as field
               { FIELD field }
  | '1'        { ONE }
  | '.'        { DOT }
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | '='        { ASSIGN }
  | '>'        { GT }
  | '<'        { LT }
  | '>' '='    { GEQ }
  | '<' '='    { LEQ }
  | '=' '='    { EQ }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { TIMES }
  | '/'        { DIV }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACKET }
  | '}'        { RBRACKET }
  | "malloc"   { MALLOC }
  | "skip"     { SKIP }
  | "while"    { WHILE }
  | "if"       { IF }
  | "else"     { ELSE }
  | "atom"     { ATOM }
  | "proc"     { PROC }
  | "null"     { NULL }
  | '|' '|' '|'   { PARALLEL }
  | eof        { raise Eof }
