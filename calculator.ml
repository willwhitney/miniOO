(* File calculator.ml *)
open Parsing;;
try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      CalculatorMENHIR.prog CalculatorLEX.token lexbuf
    with Parse_error ->
      (print_string "Syntax error ..." ; print_newline ()) ;
    clear_parser ()
  done
with CalculatorLEX.Eof ->
  ()
;;
