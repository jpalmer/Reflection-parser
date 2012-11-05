BINDIR=bin/
Parser_source=Parser/Attributes.fs  Parser/EarlyBreak.fs Parser/Cache.fs Parser/parser.fs
Jack_asm_source=Parser/Jack/AsmGrammar.fs Parser/Jack/CompileAsm.fs
FSC=mono ~/FSharp-2.0.0.0/bin/fsc.exe
FSC_FLAGS=  -g+ --optimize+ --tailcalls+ --crossoptimize+
JACKTESTDIR=Parser/JackTests
TXTS=${BINDIR}PongL.txt ${BINDIR}Max.txt ${BINDIR}Max.hack ${BINDIR}PongL.hack ${BINDIR}Pong.hack ${BINDIR}Rect.hack ${BINDIR}Pong.txt ${BINDIR}Rect.txt
$(shell mkdir -p ${BINDIR})
all: ${BINDIR}Parser.dll ${BINDIR}Jack.dll ${BINDIR}Jack_test.dll
${BINDIR}Parser.dll: ${Parser_source}
	${FSC} ${Parser_source} ${FSC_FLAGS} -a -o ${BINDIR}Parser.dll

${BINDIR}Jack.dll: ${BINDIR}Parser.dll ${Jack_asm_source}
	${FSC} ${Jack_asm_source} -r "${BINDIR}Parser.dll" -a -o ${BINDIR}Jack.dll

${TXTS}: 
	cp ${JACKTESTDIR}/*.txt bin
	cp ${JACKTESTDIR}/*.hack bin



${BINDIR}Jack_test.dll:  ${BINDIR}Jack.dll Parser/JackTests/AsmTest.fs ${TXTS}
	${FSC} Parser/JackTests/AsmTest.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll" -r "nunit.framework.dll" -a -o ${BINDIR}Jack_test.dll
