type cmdNode = Empty
  | ScopeNode of varNode * cmdNode
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
