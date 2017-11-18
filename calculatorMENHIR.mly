/* File calculatorMENHIR.mly */

%{ (* header *)

open Ast;;
open Scope;;
open Util;;
open Strutil;;

let print_tree cmd =
  printflush_str (cmd_tree cmd 0)

let heap_counter = ref 0

let create_heaploc () =
  let new_loc = !heap_counter in
  heap_counter := !heap_counter + 1;
  new_loc

let rec step ctrl state =
  let (stack, heap) = unwrap_state state in
  let heap_table = unwrap_heap heap in
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
        Hashtbl.replace heap_table new_heap_pair new_heap_entry;
        Nonterminal (CmdCtrl cmd, State (new_stack, heap))
    | CallNode (e1, e2) ->
        let e1_value = eval_expr e1 state in
        let param_value = eval_expr e2 state in
        begin match e1_value with
        (* good lord closures need a lot of unwrapping *)
        | Value (Closure (param, closure_ctrl, closure_stack)) ->
            let new_loc = Object (create_heaploc ()) in
            (* Add a new frame to the stack that holds a mapping from the
               formal parameter to a new address in the heap *)
            let param_env = Environment (param, new_loc) in
            let param_frame = CallFrame (param_env, stack) in
            let closure_stack = prepend_frame param_frame closure_stack in
            let param_value_address = (new_loc, FieldNode "val") in
            (* writes the value of the parameter its heap address *)
            Hashtbl.replace heap_table param_value_address param_value;
            let closure_state = State (closure_stack, heap) in
            step (BlockCtrl closure_ctrl) closure_state
        | _ -> ConfigError
        end
    | MallocNode (var) ->
        printflush_str "Terminating early due to not implemented: MallocNode";
        Terminal state
    | VarAssignNode (var, expr) ->
        let expr_value = eval_expr expr state in
        begin match expr_value with
        | Value v ->
            let loc = get_var_location stack var in
            let address = (loc, FieldNode "val") in
            Hashtbl.replace heap_table address expr_value;
            Terminal (State (stack, heap))
        | _ -> ConfigError
        end

    | FieldAssignNode (e1, e2, e3) ->
        printflush_str "Terminating early due to not implemented: FieldAssignNode";
        Terminal state
    | SkipNode ->
        printflush_str "Terminating early due to not implemented: SkipNode";
        Terminal state
    | SeqNode (c1, c2) ->
        let after_c1_state = iterator (Nonterminal (CmdCtrl c1, state)) in
        Nonterminal (CmdCtrl c2, after_c1_state)
        (*
        let next_config = step c1 state in
        begin match next_config with
        | Terminal state -> Nonterminal (c2, state)
        | Nonterminal (next_ctrl, state) ->
            let new_seq = SeqNode (next)
        end
        *)
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
      Value closure
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



and iterator config =
  match config with
  | Nonterminal (ctrl, state) ->
      let (stack, heap) = unwrap_state state in
      printflush_str "----------------------------------";
      printflush_str "Step!";
      printflush_str (ctrl_name ctrl 0);
      printflush_str "Stack:";
      printflush_str (stack_name stack 0);
      printflush_str "Heap:";
      printflush_str (heap_name heap);
      iterator (step ctrl state)
  | Terminal (State (stack, heap)) ->
      printflush_str "==================================";
      printflush_str "Completed execution!";
      printflush_str "Stack:";
      printflush_str (stack_name stack 0);
      printflush_str "Heap:";
      printflush_str (heap_name heap);
      printflush_str "";
      State (stack, heap)
  | ConfigError -> failwith "Error propagated up to iterator"


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
  | expr EOL     { printflush_str (expr_tree $1 0) }
  | boolean EOL     { printflush_str (bool_tree $1 0) }

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
