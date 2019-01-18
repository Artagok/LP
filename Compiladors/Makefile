all:
	antlr -gt chatbot.g
	dlg -ci parser.dlg scan.c
	g++ -o chatbot -w chatbot.c scan.c err.c
	rm -f *.o chatbot.c scan.c err.c parser.dlg tokens.h mode.h
