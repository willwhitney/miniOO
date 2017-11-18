(* static domains *)
type cmdNode = Empty
  | VardecNode of varNode * cmdNode
  | CallNode of exprNode * exprNode
  | MallocNode of varNode
  | VarAssignNode of varNode * exprNode
  | FieldAssignNode of exprNode * exprNode * exprNode
  | SkipNode
  | SeqNode of cmdNode * cmdNode
  | WhileNode of boolNode * cmdNode
  | CondNode of boolNode * cmdNode * cmdNode
  | ParallelNode of cmdNode * cmdNode
  | AtomNode of cmdNode
and exprNode =
    NumNode of int
  | MinusNode of exprNode * exprNode
  | NullNode
  | VarAccessNode of varNode
  | FieldLiteralNode of fieldNode
  | FieldAccessNode of exprNode * exprNode
  | ProcNode of varNode * cmdNode
and varNode = VarNode of string
and fieldNode = FieldNode of string
and boolNode =
    TrueNode
  | FalseNode
  | LessNode of exprNode * exprNode
;;

(* Semantic domains *)
type boolType =
  | True
  | False
  | BoolError
and objType =
    Object of int
and locType =
  | ObjLoc of objType
  | NullLoc
and valType =
  | FieldVal of fieldNode
  | IntVal of int
  | LocVal of locType
  | Closure of varNode * controlType * stackType
  (* | NullVal *)
and tvalType =
  | Value of valType
  | ValueError of string
and envType =
    Environment of varNode * objType
and frameType =
  | DeclFrame of envType
  | CallFrame of envType * stackType
and stackType =
    Stack of frameType list
and heapType =
  (* | Heap of ((objType * fieldNode) * tvalType) list *)
  Heap of ((objType * fieldNode),  tvalType) Hashtbl.t
and stateType =
  | State of stackType * heapType
and controlType =
  | CmdCtrl of cmdNode
  | BlockCtrl of controlType
and configType =
  | Nonterminal of controlType * stateType
  | Terminal of stateType
  | ConfigError of string
