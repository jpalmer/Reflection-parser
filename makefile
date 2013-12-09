BINDIR=bin/
Parser_source=Parser/Attributes.fs  Parser/EarlyBreak.fs Parser/Cache.fs Parser/parser.fs
Jack_asm_source=Parser/Jack/AsmGrammar.fs Parser/Jack/VMGrammar.fs Parser/Jack/CompileAsm.fs Parser/Jack/CompileVM.fs 
FSC=fsharpc
FSC_FLAGS=   --gccerrors -g
JACKTESTDIR=Parser/JackTests
TXTS=${BINDIR}PongL.txt ${BINDIR}Max.txt ${BINDIR}Max.hack ${BINDIR}PongL.hack ${BINDIR}Pong.hack ${BINDIR}Rect.hack ${BINDIR}Pong.txt ${BINDIR}Rect.txt
COMPILE=${FSC} ${FSC_FLAGS}
$(shell mkdir -p ${BINDIR})

all: ${BINDIR}Parser.dll ${BINDIR}Jack.dll ${BINDIR}Jack_test.dll ${BINDIR}Runner.exe

${BINDIR}Parser.dll: ${Parser_source}
	${COMPILE} ${Parser_source}  -a -o ${BINDIR}Parser.dll

${BINDIR}Jack.dll: ${BINDIR}Parser.dll ${Jack_asm_source}
	${COMPILE} ${Jack_asm_source} -r "${BINDIR}Parser.dll" -a -o ${BINDIR}Jack.dll

${BINDIR}Runner.exe: Parser/Runner/Runner.fs ${BINDIR}Parser.dll ${BINDIR}Jack.dll
	${COMPILE}  Parser/Runner/Runner.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll"  -o ${BINDIR}Runner.exe

${TXTS}: 
	cp ${JACKTESTDIR}/*.txt bin
	cp ${JACKTESTDIR}/*.hack bin

run: ${BINDIR}Runner.exe
	mono --debug ${BINDIR}Runner.exe

${BINDIR}Jack_test.dll:  ${BINDIR}Jack.dll Parser/JackTests/AsmTest.fs ${TXTS}
	${FSC} Parser/JackTests/AsmTest.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll" -r "nunit.framework.dll" -a -o ${BINDIR}Jack_test.dll
