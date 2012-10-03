runner.exe: parser.dll Parser/Runner/Runner.fs
	mono ~/FSharp-2.0.0.0/bin/fsc.exe -r "parser.dll" Parser/Runner/Runner.fs -g -o runner.exe
parser.dll: Parser/parser.fs Parser/grammar.fs Parser/Attributes.fs
	mono ~/FSharp-2.0.0.0/bin/fsc.exe Parser/Attributes.fs Parser/grammar.fs Parser/parser.fs -g -a -o parser.dll
