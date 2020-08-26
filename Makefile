a.out: syntax.l syntax.y ast.h
	bison -d syntax.y
	flex++ syntax.l
	bison -v --debug syntax.y
	g++ -c -g lex.yy.cc -o syntax.tab.cc
