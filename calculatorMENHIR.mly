/* File calculatorMENHIR.mly */

%{ (* header *)
type symbTable = (string * int) list ;;

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


type cmdNode = Empty
  | ScopeNode of varNode * cmdNode
  | CallNode of exprNode * exprNode
  | MallocNode of varNode
  | VarAssignNode of varNode * exprNode
  | FieldAssignNode of fieldNode * exprNode
  | SkipNode
  | SeqNode of cmdNode * cmdNode
  | WhileNode of boolNode * cmdNode
  | CondNode of boolNode * cmdNode * cmdNode
  | ParallelNode of cmdNode * cmdNode
  | AtomNode of cmdNode
and exprNode =
    NumNode of int
  | ArithNode of arithNode
  | NullNode
  | VarAccessNode of varNode
  | FieldAccessNode of exprNode * exprNode
  | ProcNode of varNode * cmdNode
and arithNode =
    PlusNode of exprNode * exprNode
  | MinusNode of exprNode * exprNode
  | TimesNode of exprNode * exprNode
  | DivNode of exprNode * exprNode
and varNode = VarNode of string
and fieldNode = FieldNode of string
and boolNode =
    TrueNode
  | FalseNode
  | GreaterNode of exprNode * exprNode
  | GreaterEqualNode of exprNode * exprNode
  | EqualNode of exprNode * exprNode
  | LessEqualNode of exprNode * exprNode
  | LessNode of exprNode * exprNode
;;


let printflush_int i = print_int (i); print_string "\n"; flush stdout;;
let printflush_str s = print_string (s); print_string "\n"; flush stdout;;

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

%left PLUS MINUS          /* lowest precedence  */
%left TIMES DIV           /* medium precedence  */
%nonassoc UMINUS          /* highest precedence */

%% /* rules */

prog :
    cmd EOL     { printflush_str "cmd" }
  | expr EOL     { printflush_str "expr" }
  | boolean EOL     { printflush_str "boolean" }
  | arithmetic EOL  {printflush_str "arithmetic"}


/*arithmetic :
    ONE MINUS ONE { MinusNode (1, 1) }
  | ONE PLUS ONE { PlusNode (1, 1) }
  | ONE TIMES ONE { TimesNode (1, 1) }
  | ONE DIV ONE { DivNode (1, 1) }*/

/*arithmetic :
    e1=expr MINUS e2=expr { MinusNode (e1, e2) }
  | e1=expr PLUS e2=expr { PlusNode (e1, e2) }
  | e1=expr TIMES e2=expr { TimesNode (e1, e2) }
  | e1=expr DIV e2=expr { DivNode (e1, e2) }*/

arithmetic :
    e1=expr MINUS e2=expr { ArithNode (MinusNode (e1, e2)) }
  | e1=expr PLUS e2=expr { ArithNode (PlusNode (e1, e2)) }
  | e1=expr TIMES e2=expr { ArithNode (TimesNode (e1, e2)) }
  | e1=expr DIV e2=expr { ArithNode (DivNode (e1, e2)) }

/*expr :
    ONE {printflush_str "one"; NumNode 1}
  | arithmetic { ArithNode $1}*/

expr :
    FIELD { FieldNode $1 }
  | VAR { VarNode $1 }
  | ONE { NumNode 1 }
  | NULL { NullNode }
  | arithmetic { ArithNode $1 }
  | expr DOT expr { VarAccessNode ($1, $3) }
  | PROC v=VAR COLON c=cmd { ProcNode (v, c) }

/*varDecl :
    VARDEC v=VAR  { D}*/

comparison :
    expr LT expr {printflush_str "lt"}
  | expr LEQ expr {printflush_str "leq"}
  | expr EQ expr {printflush_str "eq"}
  | expr GEQ expr {printflush_str "geq"}
  | expr GT expr {printflush_str "gt"}

boolean :
    TRUE {printflush_str "true"}
  | FALSE {printflush_str "false"}
  | comparison {printflush_str "comparison"}

parallel :
    LBRACKET cmd PARALLEL cmd RBRACKET {printflush_str "parallel"}

conditional :
    IF boolean cmd ELSE cmd {printflush_str "cond"}

loop :
    WHILE boolean cmd {printflush_str "while"}

cmdSequence :
    LBRACKET cmd SEMICOLON cmd RBRACKET {printflush_str "cmd seq"}

call :
    expr LPAREN expr RPAREN {printflush_str "call"}

cmd :
    VARDEC v=VAR SEMICOLON c=cmd { ScopeNode (v, c) }
  | call {}
  | MALLOC LPAREN VAR RPAREN {printflush_str "malloc"}
  | VAR ASSIGN expr {printflush_str "assign"}
  | expr DOT expr {printflush_str "field access"}
  | SKIP {printflush_str "skip"}
  | cmdSequence {}
  | loop {}
  | conditional {}
  | parallel {}
  | ATOM LPAREN cmd RPAREN {printflush_str "atom"}

%% (* trailer *)
