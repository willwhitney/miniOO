/* File calculatorMENHIR.mly */

%{ (* header *)
(*
type symbTable = (string * int) list ;;
let sb = ref([] : symbTable) ;;

let getvalue x =
   if (List.mem_assoc x !sb) then
     (List.assoc x !sb)
   else
     0;;

let rec except x l = match l with
  []   -> []
| h::t -> if (h = x) then t
            else h::(except x t)

let setvalue x v =
  (print_string (x ^ " = "); print_int (v);
   print_string ";\n"; flush stdout;
   if (List.mem_assoc x !sb) then
     sb := (x, v) :: (except (x, (List.assoc x !sb)) !sb)
   else
     sb := (x, v) :: !sb
  );;
*)

(*
type varType = Var of string
and fieldType = Field of string
and intType = int of int
and exprType = fieldType | varType
| intType
| Minus of exprType * exprType
| Plus of exprType * exprType
| Times of exprType * exprType
| Div of exprType * exprType
| Access of exprType * exprType
| Null

and boolType = True | False
| Lesser of exprType * exprType
| Greater of exprType * exprType
| Equal of exprType * exprType
| Leq of exprType * exprType
| Geq of exprType * exprType

and cmdType = Decl of var * cmdType
| Call of exprType * exprType
| Malloc of var
| VarAssign of var * exprType
| FieldAssign of exprType * exprType * exprType
| SKIP
| Seq of cmdType * cmdType
| While of boolType * cmdType
| Cond of boolType * cmdType * cmdType
| Parallel of cmdType * cmdType
| Atom of cmdType
;;

type programType = exprType | boolType | cmdType

type binaryNode = Node of 'a * 'a * 'a
*)

(*type 'a tree = Empty | Node of 'a * 'a * 'a*)
(*
type cmdNode = Empty
  | Node of scopeNode
  | Node of callNode
  | Node of mallocNode
  | Node of varAssignNode
  | Node of fieldAssignNode
  | Node of skipNode
  | Node of seqNode
  | Node of whileNode
  | Node of condNode
  | Node of parallelNode
  | Node of atomNode
and scopeNode = Node of declNode * cmdNode
and callNode = Node of exprNode * exprNode
and mallocNode = Node of varNode
and varAssignNode = Node of varNode * exprNode
and fieldAssignNode = Node of fieldNode * exprNode
and skipNode = SkipNode
and seqNode = Node of cmdNode * cmdNode
and whileNode = Node of boolNode * cmdNode
and condNode = Node of boolNode * cmdNode * cmdNode
and parallelNode = Node of cmdNode * cmdNode
and atomNode = Node of cmdNode
and exprNode =
    Node of numNode
  | Node of arithNode
  | Node of nullNode
  | Node of varNode
  | Node of accessNode
  | Node of procNode
and numNode = Node of int
and arithNode =
    PlusNode of exprNode * exprNode
  | MinusNode of exprNode * exprNode
  | TimesNode of exprNode * exprNode
  | DivNode of exprNode * exprNode
and nullNode = NullNode
and varNode = VarNode of string
and accessNode = Node of exprNode * exprNode
and procNode = Node of varNode * cmdNode
;;
*)

open Ast;;
open Scope;;

let printflush_int i = print_int (i); print_string "\n"; flush stdout;;
let printflush_str s = print_string (s); print_string "\n"; flush stdout;;

let rec tabs n =
  if n > 0 then
    "|" ^ "   " ^ (tabs (n-1))
  else ""

let header s n =
  (tabs n) ^ s ^ "\n"

let clear s =
  s ^ "\n"

let rec cmd_name node level =
  let nlevel = level + 1 in
  match node with
  | Empty ->
      (tabs level) ^ (clear ("Empty"))
  | ScopeNode ((VarNode s), cmd) ->
      (header ("Scope: variable " ^ s) level) ^ (cmd_name cmd nlevel)
  | CallNode (e1, e2) ->
      (header "Call" level) ^ (expr_name e1 nlevel) ^ (expr_name e2 nlevel)
  | MallocNode (var) ->
      (header "Malloc" level) ^ (var_name var nlevel)
  | VarAssignNode (var, expr) ->
      (header "VarAssign" level) ^ (var_name var nlevel) ^ (expr_name expr nlevel)
  | FieldAssignNode (e1, e2, e3) ->
      (header "FieldAssign" level) ^
      (expr_name e1 nlevel) ^ (expr_name e2 nlevel) ^ (expr_name e3 nlevel)
  | SkipNode ->
      (tabs level) ^ (clear ("Skip"))
  | SeqNode (c1, c2) ->
      (header "Seq" level) ^ (cmd_name c1 nlevel) ^ (cmd_name c2 nlevel)
  | WhileNode (boolean, cmd) ->
      (header "While" level) ^ (bool_name boolean nlevel) ^ (cmd_name cmd nlevel)
  | CondNode (boolean, c1, c2) ->
      (header "Cond" level) ^ (bool_name boolean nlevel) ^ (cmd_name c1 nlevel) ^ (cmd_name c2 nlevel)
  | ParallelNode (c1, c2) ->
      (header "Parallel" level) ^ (cmd_name c1 nlevel) ^ (cmd_name c2 nlevel)
  | AtomNode (cmd) ->
      (header "Atom" level) ^ (cmd_name cmd nlevel)

and expr_name node level =
  let nlevel = level + 1 in
  match node with
  | NumNode num ->
      (tabs level) ^ (clear ("Num: " ^ (string_of_int num)))
  | MinusNode (e1, e2) ->
      (header "Minus" level) ^ (expr_name e1 nlevel) ^ (expr_name e2 nlevel)
  | NullNode ->
      (tabs level) ^ (clear ("Null"))
  | VarAccessNode (var) ->
      (header "VarAccess" level) ^ (var_name var nlevel)
  | FieldLiteralNode (field) ->
      (header "FieldLiteral" level) ^ (field_name field nlevel)
  | FieldAccessNode (e1, e2) ->
      (header "FieldAccess" level) ^ (expr_name e1 nlevel) ^ (expr_name e2 nlevel)
  | ProcNode (var, cmd) ->
      (header "Proc" level) ^ (var_name var nlevel) ^ (cmd_name cmd nlevel)

and var_name node level =
  match node with
  | VarNode s ->
      (tabs level) ^ (clear ("Variable: " ^ s))

and field_name node level =
  match node with
  | FieldNode s ->
      (tabs level) ^ (clear ("Field: " ^ s))

and bool_name node level =
  let nlevel = level + 1 in
  match node with
  | TrueNode ->
      (tabs level) ^ (clear "True")
  | FalseNode ->
      (tabs level) ^ (clear "False")
  | LessNode (e1, e2) ->
      (header "Less" level) ^ (expr_name e1 nlevel) ^ (expr_name e2 nlevel)

let print_tree cmd =
  printflush_str (cmd_name cmd 0)


(*

printflush_str ("not found: " ^ var);

*)



let run_program cmd =
  print_tree (Scope.scope_cmd cmd [])


%} /* declarations */

/* lexer tokens */
%token EOL SEMICOLON COLON ASSIGN
%token PLUS MINUS TIMES DIV LPAREN RPAREN
%token DOT LBRACKET RBRACKET
%token GT LT GEQ LEQ EQ
%token TRUE FALSE NULL
%token MALLOC SKIP WHILE IF ELSE ATOM PROC PARALLEL
%token VARDEC
%token < string > VAR
%token < string > FIELD
%token ONE
%start prog                   /* the entry point */
%type <unit> prog
/*%type <cmdType> cmd*/
/*%type <boolType> bool*/
/*%type <exprType> expr*/

%left ASSIGN
%left PLUS MINUS          /* lowest precedence  */
%left TIMES DIV           /* medium precedence  */
%left DOT
%nonassoc UMINUS          /* highest precedence */

%% /* rules */

prog :
    cmd EOL     { run_program $1 }
  | expr EOL     { printflush_str (expr_name $1 0) }
  | boolean EOL     { printflush_str (bool_name $1 0) }

minus :
    e1=expr MINUS e2=expr { MinusNode (e1, e2) }

expr :
    FIELD { FieldLiteralNode (FieldNode $1) }
  | VAR { VarAccessNode (VarNode $1) }
  | ONE { NumNode 1 }
  | NULL { NullNode }
  | minus { $1 }
  | expr DOT expr { FieldAccessNode ($1, $3) }
  | PROC v=VAR COLON c=cmd { ProcNode (VarNode v, c) }

boolean :
    TRUE { TrueNode }
  | FALSE { FalseNode}
  | expr LT expr { LessNode ($1, $3) }

parallel :
    LBRACKET cmd PARALLEL cmd RBRACKET { ParallelNode ($2, $4) }

conditional :
    IF boolean cmd ELSE cmd { CondNode ($2, $3, $5) }

loop :
    WHILE boolean cmd { WhileNode ($2, $3) }

cmdSequence :
    LBRACKET cmd SEMICOLON cmd RBRACKET { SeqNode ($2, $4)}

call :
    expr LPAREN expr RPAREN { CallNode ($1, $3)}


cmd :
    VARDEC v=VAR SEMICOLON c=cmd {ScopeNode (VarNode v, c) }
  | call { $1 }
  | MALLOC LPAREN VAR RPAREN { MallocNode (VarNode $3) }
  | VAR ASSIGN expr { VarAssignNode (VarNode $1, $3) }
  | expr DOT expr ASSIGN expr { FieldAssignNode ($1, $3, $5) }
  | SKIP { SkipNode }
  | cmdSequence { $1 }
  | loop { $1 }
  | conditional { $1 }
  | parallel { $1 }
  | ATOM LPAREN cmd RPAREN { AtomNode $3 }

%% (* trailer *)
