all:
	@echo "Making all."
	@make delete
	@ocamllex calculatorLEX.mll
	@echo "about to do mly"
	@menhir calculatorMENHIR.mly
	@# ocamlyacc calculatorMENHIR.mly
	@echo "finished compiling individual files"
	@ocamlc -c ast.ml
	@ocamlc -c scope.ml
	@ocamlc -c util.ml
	@ocamlc -c strutil.ml
	@echo "finished strutil"
	@ocamlc -c calculatorMENHIR.mli
	@ocamlc -c calculatorLEX.ml
	@ocamlc -c calculatorMENHIR.ml
	@ocamlc -c calculator.ml
	@ocamlc -o calculator ast.cmo scope.cmo util.cmo strutil.cmo calculatorLEX.cmo calculatorMENHIR.cmo calculator.cmo
	@echo "Completed compilation.\n"
	@#echo "var X; {X=1; var X; X=1}" | ./calculator
	@#echo "var X; {var X; X = proc Y: X=Y; X=1 }" | ./calculator
	@echo "var X; X = proc Y: X=1"
	@echo "var X; {X = proc Y: X=1; X(1)}" | ./calculator

# build-and-notify:
# 	-@make all
# 	@make all \
# 		| xargs -I XXX sh -c "echo 'XXX' && osascript -e 'display notification \"XXX\" with title \"Document generation output\"';"

delete:
	@/bin/rm -f calculator calculator.cmi calculator.cmo calculatorLEX.cmi @calculatorLEX.cmo calculatorLEX.ml calculatorMENHIR.cmi @calculatorMENHIR.cmo calculatorMENHIR.ml calculatorMENHIR.mli makefile~

watch:
	@make all
	@fswatch --event=Updated "calculatorMENHIR.mly" "scope.ml" "ast.ml" "strutil.ml" "util.ml" \
		| xargs -I XXX make all
