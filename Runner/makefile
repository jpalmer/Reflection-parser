${BINDIR}Runner.exe: Runner.fs ${BINDIR}Parser.dll ${BINDIR}Jack.dll
	${COMPILE}  Runner.fs -r "${BINDIR}Parser.dll" -r "${BINDIR}Jack.dll"  -o ${BINDIR}Runner.exe
