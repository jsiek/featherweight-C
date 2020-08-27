a.out: syntax.l syntax.y ast.h ast.cc typecheck.cc
	bison -d syntax.y
	flex syntax.l
	bison -v --debug syntax.y  -o syntax.tab.cc
	g++ -std=c++11 -Wno-deprecated-register -c -g lex.yy.c
	g++ -std=c++11 -c -g ast.cc
	g++ -std=c++11 -c -g typecheck.cc
	g++ -std=c++11 -g lex.yy.o ast.o typecheck.o syntax.tab.cc
