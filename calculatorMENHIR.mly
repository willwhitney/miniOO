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

let allocated_vars = Hashtbl.create 100

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
        let val_address = (new_loc, FieldNode "val") in
        let new_heap_entry = Value (LocVal NullLoc) in
        Hashtbl.replace heap_table val_address new_heap_entry;
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
            let param_val_address = (new_loc, FieldNode "val") in
            (* writes the value of the parameter its heap address *)
            Hashtbl.replace heap_table param_val_address param_value;
            let closure_state = State (closure_stack, heap) in
            step (BlockCtrl closure_ctrl) closure_state
        | ValueError s ->
            let errmsg = "e1 was not a value in CallNode:\n" ^ s in
            ConfigError errmsg
        | _ -> ConfigError "e1 was not a closure in CallNode"
        end

    | MallocNode (var) ->
        let loc = get_var_location stack var in
        let new_heap_loc_obj = Object (create_heaploc ()) in
        let new_heap_loc_val = Value (LocVal (ObjLoc new_heap_loc_obj)) in
        let val_address = (loc, FieldNode "val") in
        Hashtbl.replace allocated_vars new_heap_loc_obj true;
        Hashtbl.replace heap_table val_address new_heap_loc_val;
        Terminal state

    | VarAssignNode (var, expr) ->
        let expr_value = eval_expr expr state in
        begin match expr_value with
        | Value v ->
            let loc = get_var_location stack var in
            let val_address = (loc, FieldNode "val") in
            Hashtbl.replace heap_table val_address expr_value;
            Terminal (State (stack, heap))
        | ValueError s ->
            let errmsg = "expr was not a value in VarAssign:\n" ^ s in
            ConfigError errmsg
        end

    | FieldAssignNode (e1, e2, e3) ->
        let e1_value = eval_expr e1 state in
        let e2_value = eval_expr e2 state in
        let e3_value = eval_expr e3 state in
        begin match e1_value with
        | Value (LocVal (ObjLoc l)) ->
          if Hashtbl.mem allocated_vars l then
            begin match e2_value with
            | Value (FieldVal f) ->
                let address = (l, f) in
                Hashtbl.replace heap_table address e3_value;
                Terminal state
            | ValueError s ->
                let errmsg = "field in FieldAssign was not a value:\n" ^ s in
                ConfigError errmsg
            | _ -> ConfigError "field in FieldAssign was not a field type"
            end
          else
            let errmsg = Printf.sprintf "%s has not been allocated before field assignment" (obj_name l) in
            ConfigError errmsg
        | ValueError s ->
            let errmsg = "Location in FieldAssign was not a value:\n" ^ s in
            ConfigError errmsg
        | _ -> ConfigError "location in FieldAssign was not a location type"
        end

    | SkipNode ->
        Terminal state

    | SeqNode (c1, c2) ->
        let after_c1_state = iterator (Nonterminal (CmdCtrl c1, state)) in
        Nonterminal (CmdCtrl c2, after_c1_state)

    (* represent While (boolean, cmd) with a transformation into:
        if boolean:
          cmd;
          while boolean:
            cmd;
        else:
          empty
    *)
    | WhileNode (boolean, cmd) ->
        let true_cmd_step = cmd in
        let true_cmd_repeat = WhileNode (boolean, cmd) in
        let true_cmd = SeqNode (true_cmd_step, true_cmd_repeat) in
        let false_cmd = Empty in
        let cond_cmd = CondNode (boolean, true_cmd, false_cmd) in
        Nonterminal (CmdCtrl cond_cmd, state)

    | CondNode (boolean, c1, c2) ->
        let bool_val = eval_bool boolean state in
        begin match bool_val with
        | True -> Nonterminal (CmdCtrl c1, state)
        | False -> Nonterminal (CmdCtrl c2, state)
        | BoolError s ->
            ConfigError ("Boolean in CondNode was error:\n" ^ s)
        end

    | ParallelNode (c1, c2) ->
        Nonterminal (CmdCtrl (SeqNode (c1, c2)), state)

    | AtomNode (cmd) ->
        Terminal (iterator (Nonterminal (CmdCtrl cmd, state)))

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
    | ConfigError s -> ConfigError s
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
          | ValueError s ->
              let errmsg = "Second argument of minus was not a value:\n" ^ s in
              ValueError errmsg
          | _ -> ValueError "e2 was not an int in minus"
          end
      | ValueError s ->
        let errmsg = "First arg of minus was not a value:\n" ^ s in
        ValueError errmsg
      | _ -> ValueError "e1 was not an int in minus"
      end
  | NullNode -> Value (LocVal NullLoc)
  | VarAccessNode var ->
      let var_loc = get_var_location stack var in
      Hashtbl.find heap_table (var_loc, FieldNode "val")
  | FieldLiteralNode field -> Value (FieldVal field)
  | FieldAccessNode (e1, e2) ->
      let loc = eval_expr e1 state in
      let field = eval_expr e2 state in
      begin match loc with
      | Value (LocVal (ObjLoc l)) ->
          if Hashtbl.mem allocated_vars l then
            begin match field with
            | Value (FieldVal f) ->
                if Hashtbl.mem heap_table (l, f) then
                  Hashtbl.find heap_table (l, f)
                else
                  Value (LocVal NullLoc)
                  (*let errmsg = Printf.sprintf "Address (%s, %s) did not exist on the heap (@FieldAccess)" (obj_name l) (field_name f) in
                  ValueError errmsg*)
            | ValueError s ->
                let errmsg = "field in FieldAccess was not a value:\n" ^ s in
                ValueError errmsg
            | _ -> ValueError "field in FieldAccess was not a field type"
            end
          else
            ValueError ("Variable " ^ (obj_name l) ^
                        " was not malloc'd before FieldAccess")
      | ValueError s ->
          let errmsg = "Location in FieldAccess was not a value:\n" ^ s in
          ValueError errmsg
      | _ -> ValueError "location in FieldAccess was not a location type"
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
          | ValueError s -> BoolError ("Number 2 in LessNode was error:\n" ^ s)
          | _ -> BoolError ("Number 2 in LessNode was not an IntVal")
          end
      | ValueError s -> BoolError ("Number 1 in LessNode was error:\n" ^ s)
      | _ -> BoolError ("Number 1 in LessNode was not an IntVal")
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
      printflush_str ("Allocated: " ^ (allocateds_name allocated_vars));
      iterator (step ctrl state)
  | Terminal (State (stack, heap)) ->
      printflush_str "==================================";
      printflush_str "Completed execution!";
      printflush_str "Stack:";
      printflush_str (stack_name stack 0);
      printflush_str "Heap:";
      printflush_str (heap_name heap);
      printflush_str ("Allocated: " ^ (allocateds_name allocated_vars));
      printflush_str "";
      State (stack, heap)
  | ConfigError s -> failwith ("Error propagated up to iterator:\n" ^ s)


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
