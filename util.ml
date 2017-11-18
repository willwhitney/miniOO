open Ast
open Scope

let unwrap_state state = match state with State (stack, heap) -> (stack, heap)
let unwrap_heap heap = match heap with Heap h -> h
let unwrap_stack stack = match stack with Stack s -> s

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
        | CallFrame (Environment (frameVar, obj), stack) ->
            if var = frameVar then obj
            else get_var_location stack_tail var
      end
  | _ -> failwith ("Variable not found in stack: " ^ varname)

let prepend_frame frame (Stack stack) = Stack (frame::stack)
