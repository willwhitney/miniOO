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
  | VardecNode ((VarNode s), cmd) ->
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

let heap_counter = ref 0

let unwrap_state state = match state with State (stack, heap) -> (stack, heap)
let unwrap_heap heap = match heap with Heap h -> h
let unwrap_stack stack = match stack with Stack s -> s

let rec stack_name stack level =
  let stacklist = unwrap_stack stack in
  match stacklist with
  | [] -> ""
  | f::stack_tail ->
      let stack_tail = Stack stack_tail in
      (frame_name f level) ^ "\n" ^ (stack_name stack_tail level)
and frame_name frame level =
  let nlevel = level + 1 in
  match frame with
  | DeclFrame env -> "Declaration: " ^ (env_name env)
  | CallFrame (env, stack) ->
      "Call: " ^ (env_name env) ^ " Calling stack:" ^ (stack_name stack nlevel)
and env_name env =
  match env with Environment (VarNode varname, Object loc) ->
    "Env: " ^ varname ^ " -> " ^ (string_of_int loc)


let rec get_var_location stack var =
  let varname = match var with VarNode s -> s in
  let stack = unwrap_stack stack in
  match stack with
  | f::stack_tail ->
      let stack_tail = Stack stack_tail in
      begin match f with
        | DeclFrame (Environment (frameVar, obj)) ->
            if var = frameVar then obj
            else get_var_location stack_tail var
        | _ -> get_var_location stack_tail var
      end
  | _ -> failwith ("Variable not found in stack: " ^ varname)

(*
| CallNode (e1, e2) ->
| MallocNode (var) ->
| VarAssignNode (var, expr) ->
| FieldAssignNode (e1, e2, e3) ->
| SkipNode ->
| SeqNode (c1, c2) ->
| WhileNode (boolean, cmd) ->
| CondNode (boolean, c1, c2) ->
| ParallelNode (c1, c2) ->
| AtomNode (cmd) ->


let () = match config with
  | (Nonterminal (ctrl, state)) ->
      (ctrl, state)
  | _ -> failwith "step called on terminal state"
in

*)

let create_heaploc () =
  let new_loc = !heap_counter in
  heap_counter := !heap_counter + 1;
  new_loc

let prepend_frame frame (Stack stack) = Stack (frame::stack)

let rec iterator config =
  match config with
  | Nonterminal (ctrl, state) ->
      iterator (step ctrl state)
  | Terminal (State (stack, heap)) ->
      printflush_str ("Completed execution! \nStack: \n" ^ (stack_name stack 0));
      State (stack, heap)
  | ConfigError -> failwith "Error propagated up to iterator"

and step ctrl state =
  let (stack, heap) = unwrap_state state in
  let heap_table = unwrap_heap heap in
  printflush_str (stack_name stack 0);
  match ctrl with
  | CmdCtrl cmd ->
    begin match cmd with
    | Empty -> Terminal state
    | VardecNode (var, cmd) ->
        let new_loc = Object (create_heaploc ()) in
        let new_frame = DeclFrame (Environment (var, new_loc)) in
        let new_stack = prepend_frame new_frame stack in
        let new_heap_pair = (new_loc, FieldNode "val") in
        let new_heap_entry = Value (LocVal NullLoc) in
        Hashtbl.add heap_table new_heap_pair new_heap_entry;
        Nonterminal (CmdCtrl cmd, State (new_stack, heap))
    | CallNode (e1, e2) ->
        let e1_value = eval_expr e1 state in
        let param_value = eval_expr e2 state in
        begin match e1_value with
        | Value (CloVal (Closure (param, closure_ctrl, closure_stack))) ->
        (*
        | Value (closureValue) ->
            match closureValue with CloVal (closure) ->
            match closure with Closure (param, closure_ctrl, closure_stack) ->
            *)
            (*(CloVal (Closure (param, closure_ctrl, closure_stack)))*)
            let new_loc = Object (create_heaploc ()) in
            (* Add a new frame to the stack that holds a mapping from the
               formal parameter to a new address in the heap *)
            let param_env = Environment (param, new_loc) in
            let param_frame = CallFrame (param_env, stack) in
            let closure_stack = prepend_frame param_frame closure_stack in
            let param_value_address = (new_loc, FieldNode "val") in
            (* writes the value of the parameter its heap address *)
            Hashtbl.add heap_table param_value_address param_value;
            let closure_state = State (closure_stack, heap) in
            step (BlockCtrl closure_ctrl) closure_state
        | _ -> ConfigError
        end
    | MallocNode (var) ->
        printflush_str "Terminating early due to not implemented: MallocNode";
        Terminal state
    | VarAssignNode (var, expr) ->
        printflush_str "Terminating early due to not implemented: VarAssignNode";
        Terminal state
    | FieldAssignNode (e1, e2, e3) ->
        printflush_str "Terminating early due to not implemented: FieldAssignNode";
        Terminal state
    | SkipNode ->
        printflush_str "Terminating early due to not implemented: SkipNode";
        Terminal state
    | SeqNode (c1, c2) ->
        printflush_str "Terminating early due to not implemented: SeqNode";
        Terminal state
    | WhileNode (boolean, cmd) ->
        printflush_str "Terminating early due to not implemented: WhileNode";
        Terminal state
    | CondNode (boolean, c1, c2) ->
        printflush_str "Terminating early due to not implemented: CondNode";
        Terminal state
    | ParallelNode (c1, c2) ->
        printflush_str "Terminating early due to not implemented: ParallelNode";
        Terminal state
    | AtomNode (cmd) ->
        printflush_str "Terminating early due to not implemented: AtomNode";
        Terminal state
    | _ ->
        printflush_str "Terminating prematurely due to unimplemented cmd case.";
        Terminal state
    end
  | BlockCtrl blocked_ctrl ->
    let next_config = step blocked_ctrl state in
    begin match next_config with
    | Nonterminal (next_ctrl, next_state) ->
      Nonterminal (BlockCtrl next_ctrl, next_state)
    | Terminal (State (next_stack, next_heap)) ->
      let stack_list = unwrap_stack next_stack in
      begin match stack_list with
      | frame::stack_tail ->
          begin match frame with
          | DeclFrame env ->
              Terminal (State (Stack stack_tail, next_heap))
          | CallFrame (env, outside_stack) ->
              Terminal (State (outside_stack, next_heap))
          end
      | _ -> failwith "Tried to pop empty stack."
      end
    | ConfigError -> ConfigError
    end

and eval_expr expr state =
  let (stack, heap) = unwrap_state state in
  let heap_table = unwrap_heap heap in
  match expr with
  | NumNode num -> Value (IntVal num)
  | MinusNode (e1, e2) ->
      let e1_value = eval_expr e1 state in
      let e2_value = eval_expr e2 state in
      begin match e1_value with
      | Value (IntVal num1) ->
          begin match e2_value with
          | Value (IntVal num2) ->
              let result = num1 - num2 in
              Value (IntVal result)
          | _ -> ValueError
          end
      | _ -> ValueError
      end
  | NullNode -> Value NullVal
  | VarAccessNode var ->
      let var_loc = get_var_location stack var in
      Hashtbl.find heap_table (var_loc, FieldNode "val")
  | FieldLiteralNode field -> Value (FieldVal field)
  | FieldAccessNode (e1, e2) ->
      let loc = eval_expr e1 state in
      let field = eval_expr e2 state in
      begin match loc with
      | Value (LocVal (ObjLoc l)) ->
          begin match field with
          | Value (FieldVal f) ->
              if Hashtbl.mem heap_table (l, f) then
                Hashtbl.find heap_table (l, f)
              else ValueError
          | _ -> ValueError
          end
      | _ -> ValueError
      end
  | ProcNode (var, cmd) ->
      let closure = Closure (var, CmdCtrl cmd, stack) in
      Value (CloVal closure)
and eval_bool boolean state =
  match boolean with
  | TrueNode -> True
  | FalseNode -> False
  | LessNode (e1, e2) ->
      let e1_value = eval_expr e1 state in
      let e2_value = eval_expr e2 state in
      begin match e1_value with
      | Value (IntVal num1) ->
          begin match e2_value with
          | Value (IntVal num2) ->
              if num1 < num2 then True
              else False
          | _ -> BoolError
          end
      | _ -> BoolError
      end



(*

printflush_str ("not found: " ^ var);

*)

let run_program cmd =
  let scoped_cmd = Scope.scope_cmd cmd [] in
  print_tree scoped_cmd;
  let initial_stack = Stack [] in
  let initial_heap = Heap (Hashtbl.create 100) in
  let initial_state = State (initial_stack, initial_heap) in
  iterator (Nonterminal (CmdCtrl scoped_cmd, initial_state));
  ()


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
    VARDEC v=VAR SEMICOLON c=cmd {VardecNode (VarNode v, c) }
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
