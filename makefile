BINDIR=bin/
Parser_source=Parser/Attributes.fs  Parser/EarlyBreak.fs Parser/Cache.fs Parser/parser.fs
Jack_asm_source=Parser/Jack/AsmGrammar.fs Parser/Jack/CompileAsm.fs
FSC=mono ~/FSharp-2.0.0.0/bin/fsc.exe
FSC_FLAGS=-g  -g --optimize+ --tailcalls+ --crossoptimize+
$(shell mkdir -p ${BINDIR})
all: ${BINDIR}Parser.dll ${BINDIR}Jack.dll ${BINDIR}Jack_test.dll
${BINDIR}Parser.dll: ${Parser_source}
	${FSC} ${Parser_source} ${FSC_FLAGS} -a -o ${BINDIR}Parser.dll

${BINDIR}Jack.dll: ${BINDIR}Parser.dll ${Jack_asm_source}
	${FSC} ${Jack_asm_source} -r "${BINDIR}Parser.dll" -a -o ${BINDIR}Jack.dll

${BINDIR}Jack_test.dll:  ${BINDIR}Jack.dll Parser/JackTests/JackTest.fs
	${FSC} Parser/JackTests/JackTest.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll" -r "nunit.framework.dll" -a -o ${BINDIR}Jack_test.dll
