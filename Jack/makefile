Jack_asm_source=AsmGrammar.fs VMGrammar.fs CompileAsm.fs CompileVM.fs PrintAsm.fs

${BINDIR}Jack.dll: ${BINDIR}Parser.dll ${Jack_asm_source}
	${COMPILE} ${Jack_asm_source} -r "${BINDIR}Parser.dll" -a -o ${BINDIR}Jack.dll
