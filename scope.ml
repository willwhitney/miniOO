open Ast;;

let var_counts = Hashtbl.create 100;;

let create_var name scope =
  let id =
    if not (Hashtbl.mem var_counts name) then
      0
    else
      (Hashtbl.find var_counts name) + 1
    in
  Hashtbl.add var_counts name id;
  let new_var = name ^ (string_of_int id) in
  let new_scope = (name, new_var)::scope in
  (new_var, new_scope)
;;

let rec scope_cmd cmd scope =
match cmd with
| Empty -> Empty
| ScopeNode ((VarNode var), cmd) ->
    let (new_var, new_scope) = create_var var scope in
    let scoped_cmd = (scope_cmd cmd new_scope) in
    ScopeNode ((VarNode new_var), scoped_cmd)
| CallNode (e1, e2) -> CallNode ((scope_expr e1 scope), (scope_expr e2 scope))
| MallocNode (var) -> MallocNode (scope_var var scope)
| VarAssignNode (var, expr) ->
    VarAssignNode ((scope_var var scope), (scope_expr expr scope))
| FieldAssignNode (e1, e2, e3) ->
    FieldAssignNode ((scope_expr e1 scope), (scope_expr e2 scope), (scope_expr e3 scope))
| SkipNode -> SkipNode
| SeqNode (c1, c2) -> SeqNode (scope_cmd c1 scope, scope_cmd c2 scope)
| WhileNode (boolean, cmd) -> WhileNode (scope_bool boolean scope, scope_cmd cmd scope)
| CondNode (boolean, c1, c2) ->
    CondNode (scope_bool boolean scope, scope_cmd c1 scope, scope_cmd c2 scope)
| ParallelNode (c1, c2) -> ParallelNode (scope_cmd c1 scope, scope_cmd c2 scope)
| AtomNode (cmd) -> AtomNode (scope_cmd cmd scope)

and scope_expr expr scope =
match expr with
| NumNode num -> NumNode num
| MinusNode (e1, e2) -> MinusNode (scope_expr e1 scope, scope_expr e2 scope)
| NullNode -> NullNode
| VarAccessNode (var) -> VarAccessNode (scope_var var scope)
| FieldLiteralNode (field) -> FieldLiteralNode field
| FieldAccessNode (e1, e2) ->
    FieldAccessNode (scope_expr e1 scope, scope_expr e2 scope)
| ProcNode ((VarNode var), cmd) ->
    let (new_var, new_scope) = create_var var scope in
    ProcNode ((VarNode new_var), scope_cmd cmd new_scope)

and scope_bool boolean scope =
  match boolean with
  | TrueNode -> TrueNode
  | FalseNode -> FalseNode
  | LessNode (e1, e2) ->
      LessNode (scope_expr e1 scope, scope_expr e2 scope)

and scope_var var scope =
  match var with
  | VarNode s ->
      if List.mem_assoc s scope then
        VarNode (List.assoc s scope)
      else
        failwith ("Variable not declared: " ^ s)
