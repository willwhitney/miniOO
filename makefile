all:
	@echo "Making all."
	@make delete
	@ocamllex calculatorLEX.mll
	@menhir calculatorMENHIR.mly
	@# ocamlyacc calculatorMENHIR.mly
	@ocamlc -c calculatorMENHIR.mli
	@ocamlc -c calculatorLEX.ml
	@ocamlc -c calculatorMENHIR.ml
	@ocamlc -c calculator.ml
	@ocamlc -o calculator calculatorLEX.cmo calculatorMENHIR.cmo calculator.cmo
	@echo "Completed compilation."

# build-and-notify:
# 	-@make all
# 	@make all \
# 		| xargs -I XXX sh -c "echo 'XXX' && osascript -e 'display notification \"XXX\" with title \"Document generation output\"';"

delete:
	@/bin/rm -f calculator calculator.cmi calculator.cmo calculatorLEX.cmi @calculatorLEX.cmo calculatorLEX.ml calculatorMENHIR.cmi @calculatorMENHIR.cmo calculatorMENHIR.ml calculatorMENHIR.mli makefile~

watch:
	@make all
	@fswatch --event=Updated "calculatorMENHIR.mly" \
		| xargs -I XXX make all
