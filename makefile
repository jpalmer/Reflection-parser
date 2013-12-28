export BINDIR=${shell pwd}/bin/
FSC=fsharpc
FSC_FLAGS=   --gccerrors -g --nologo
JACKTESTDIR=Parser/JackTests
TXTS=${BINDIR}PongL.txt ${BINDIR}Max.txt ${BINDIR}Max.hack ${BINDIR}PongL.hack ${BINDIR}Pong.hack ${BINDIR}Rect.hack ${BINDIR}Pong.txt ${BINDIR}Rect.txt
export COMPILE=${FSC} ${FSC_FLAGS}
$(shell mkdir -p ${BINDIR})
export JACKTEST=${BINDIR}Jack-test.dll
ALLBINS=${BINDIR}Parser.dll ${BINDIR}Jack.dll ${JACKTEST} ${BINDIR}Runner.exe
.PHONY: all ${ALLBINS} run

all: ${ALLBINS}

run: ${ALLBINS}
	mono --debug bin/Runner.exe

${BINDIR}Parser.dll:
	cd Parser && ${MAKE}

${BINDIR}Jack.dll: ${BINDIR}Parser.dll 
	cd Jack && ${MAKE}

${BINDIR}Runner.exe: ${BINDIR}Parser.dll ${BINDIR}Jack.dll
	cd Runner && ${MAKE} 

${TXTS}: 
	cp ${JACKTESTDIR}/*.txt bin
	cp ${JACKTESTDIR}/*.hack bin

run: ${BINDIR}Runner.exe
	mono --debug ${BINDIR}Runner.exe
clean:
	rm ${ALLBINS}

${JACKTEST}:  ${BINDIR}Jack.dll ${TXTS}
	cd Parser/JackTests && ${MAKE}
