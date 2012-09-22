exe.exe: parser.fs grammar.fs
	mono ~/FSharp-2.0.0.0/bin/fsc.exe grammar.fs parser.fs -g -o exe.exe
