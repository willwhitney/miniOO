

open Ast
open Scope
open Util

let printflush_int i = print_int (i); print_string "\n"; flush stdout;;
let printflush_str s = print_string (s); print_string "\n"; flush stdout;;

let rec tabs n =
  if n > 0 then
    " " ^ "   " ^ (tabs (n-1))
  else ""

let header s n =
  (tabs n) ^ s ^ "\n"

let clear s =
  s ^ "\n"

let rec cmd_tree node level =
  let nlevel = level + 1 in
  match node with
  | Empty ->
      (tabs level) ^ (clear ("Empty"))
  | VardecNode ((VarNode s), cmd) ->
      (header ("Scope: variable " ^ s) level) ^ (cmd_tree cmd nlevel)
  | CallNode (e1, e2) ->
      (header "Call" level) ^ (expr_tree e1 nlevel) ^ (expr_tree e2 nlevel)
  | MallocNode (var) ->
      (header "Malloc" level) ^ (var_tree var nlevel)
  | VarAssignNode (var, expr) ->
      (header "VarAssign" level) ^ (var_tree var nlevel) ^ (expr_tree expr nlevel)
  | FieldAssignNode (e1, e2, e3) ->
      (header "FieldAssign" level) ^
      (expr_tree e1 nlevel) ^ (expr_tree e2 nlevel) ^ (expr_tree e3 nlevel)
  | SkipNode ->
      (tabs level) ^ (clear ("Skip"))
  | SeqNode (c1, c2) ->
      (header "Seq" level) ^ (cmd_tree c1 nlevel) ^ (cmd_tree c2 nlevel)
  | WhileNode (boolean, cmd) ->
      (header "While" level) ^ (bool_tree boolean nlevel) ^ (cmd_tree cmd nlevel)
  | CondNode (boolean, c1, c2) ->
      (header "Cond" level) ^ (bool_tree boolean nlevel) ^
          (cmd_tree c1 nlevel) ^ (cmd_tree c2 nlevel)
  | ParallelNode (c1, c2) ->
      (header "Parallel" level) ^ (cmd_tree c1 nlevel) ^ (cmd_tree c2 nlevel)
  | AtomNode (cmd) ->
      (header "Atom" level) ^ (cmd_tree cmd nlevel)
(* let cmd_name cmd = *)

and expr_tree node level =
  let nlevel = level + 1 in
  match node with
  | NumNode num ->
      (tabs level) ^ (clear ("Num: " ^ (string_of_int num)))
  | MinusNode (e1, e2) ->
      (header "Minus" level) ^ (expr_tree e1 nlevel) ^ (expr_tree e2 nlevel)
  | NullNode ->
      (tabs level) ^ (clear ("Null"))
  | VarAccessNode (var) ->
      (header "VarAccess" level) ^ (var_tree var nlevel)
  | FieldLiteralNode (field) ->
      (header "FieldLiteral" level) ^ (field_tree field nlevel)
  | FieldAccessNode (e1, e2) ->
      (header "FieldAccess" level) ^ (expr_tree e1 nlevel) ^
          (expr_tree e2 nlevel)
  | ProcNode (var, cmd) ->
      (header "Proc" level) ^ (var_tree var nlevel) ^ (cmd_tree cmd nlevel)

and var_name var =
  match var with VarNode s -> "Variable: " ^ s
and var_tree node level =
  (tabs level) ^ (clear (var_name node))

and field_name field =
  match field with FieldNode s -> "Field: " ^ s
and field_tree node level =
  (tabs level) ^ (clear (field_name node))

and bool_tree node level =
  let nlevel = level + 1 in
  match node with
  | TrueNode ->
      (tabs level) ^ (clear "True")
  | FalseNode ->
      (tabs level) ^ (clear "False")
  | LessNode (e1, e2) ->
      (header "Less" level) ^ (expr_tree e1 nlevel) ^ (expr_tree e2 nlevel)

and stack_name stack level =
  let stacklist = match stack with Stack s -> s in
  match stacklist with
  | [] -> ""
  | f::stack_tail ->
      let rest_stack = Stack stack_tail in
      let tail_str =
        if (List.length stack_tail) >= 1 then
          "\n" ^ (stack_name rest_stack level)
        else
          ""
      in
      (header ((frame_name f level) ^ tail_str) level)
and frame_name frame level =
  let nlevel = level + 1 in
  match frame with
  | DeclFrame env -> "DeclFrame: " ^ (env_name env)
  | CallFrame (env, stack) ->
      "Call: [" ^ (env_name env) ^ "] Calling stack:\n" ^ (stack_name stack nlevel)
and env_name env =
  match env with Environment (VarNode varname, Object loc) ->
    varname ^ " → " ^ (string_of_int loc)
    (* "Env: " ^ varname ^ " → " ^ (string_of_int loc) *)


and heap_name heap =
  let heap_table = unwrap_heap heap in
  Hashtbl.fold heap_name_reducer heap_table ""
  (* Hashtbl.iter heap_element_name heap_table *)
and heap_name_reducer k v acc =
  acc ^ (heap_element_name k v) ^ "\n"
and heap_element_name (Object num, FieldNode field_str) value =
  let obj_str = string_of_int num in
  (* let field_str = field_name field in *)
  Printf.sprintf "(%s, %s) → %s" obj_str field_str (val_name value)

and obj_name (Object num) =
  "Object: " ^ (string_of_int num)
and loc_name loc =
  match loc with
  | ObjLoc obj -> "ObjLoc: " ^ obj_name obj
  | NullLoc -> "NullLoc"
and val_name value =
  match value with
  | Value (FieldVal field) -> "Value " ^ (field_name field)
  | Value (IntVal num) -> "IntVal " ^ (string_of_int num)
  | Value (LocVal loc) -> "LocVal " ^ (loc_name loc)
  | Value (Closure (var, ctrl, stack)) ->
      let var_str = var_name var in
      let ctrl_str = ctrl_name ctrl 3 in
      let stack_str = stack_name stack 3 in
      Printf.sprintf "\n    Closure: \n\tVar: %s, \n\tCtrl:\n%s \tStack:\n%s" var_str ctrl_str stack_str
  | Value NullVal -> "NullVal"
  | ValueError -> "ValueError"
and ctrl_name ctrl level =
  match ctrl with
  | CmdCtrl cmd -> (tabs level) ^ ("CmdCtrl: \n" ^ (cmd_tree cmd (level + 1)))
  | BlockCtrl ctrl -> "BlockCtrl: \n" ^ (ctrl_name ctrl (level + 1))
