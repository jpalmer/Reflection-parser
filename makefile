export BINDIR=${shell pwd}/bin/
FSC=fsharpc
FSC_FLAGS=   --gccerrors -g --nologo
JACKTESTDIR=Parser/JackTests
TXTS=${BINDIR}PongL.txt ${BINDIR}Max.txt ${BINDIR}Max.hack ${BINDIR}PongL.hack ${BINDIR}Pong.hack ${BINDIR}Rect.hack ${BINDIR}Pong.txt ${BINDIR}Rect.txt
export COMPILE=${FSC} ${FSC_FLAGS}
$(shell mkdir -p ${BINDIR})

ALLBINS=${BINDIR}Parser.dll ${BINDIR}Jack.dll ${BINDIR}Jack_test.dll ${BINDIR}Runner.exe

all: ${ALLBINS}

${BINDIR}Parser.dll:
	cd Parser && make

${BINDIR}Jack.dll: ${BINDIR}Parser.dll 
	cd Jack && make

${BINDIR}Runner.exe: Parser/Runner/Runner.fs ${BINDIR}Parser.dll ${BINDIR}Jack.dll
	${COMPILE}  Parser/Runner/Runner.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll"  -o ${BINDIR}Runner.exe

${TXTS}: 
	cp ${JACKTESTDIR}/*.txt bin
	cp ${JACKTESTDIR}/*.hack bin

run: ${BINDIR}Runner.exe
	mono --debug ${BINDIR}Runner.exe
clean:
	rm ${ALLBINS}

${BINDIR}Jack_test.dll:  ${BINDIR}Jack.dll Parser/JackTests/AsmTest.fs ${TXTS}
	${COMPILE} Parser/JackTests/AsmTest.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll" -r "nunit.framework.dll" -a -o ${BINDIR}Jack_test.dll
