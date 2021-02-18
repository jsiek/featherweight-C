a.out: syntax.l syntax.y ast.h ast.cc typecheck.cc interp.cc cons_list.h assoc_list.h
	bison -d syntax.y
	flex syntax.l
	bison -v --debug syntax.y  -o syntax.tab.cc
	g++ -std=c++11 -Wno-deprecated-register -c -g lex.yy.c
	g++ -std=c++11 -c -g ast.cc
	g++ -std=c++11 -c -g typecheck.cc
	g++ -std=c++11 -c -g interp.cc
	g++ -std=c++11 -g lex.yy.o ast.o typecheck.o interp.o syntax.tab.cc

clean:
	rm -f *.o
	rm -f syntax.tab.h
	rm -f syntax.tab.cc syntax.tab.c syntax.output
	rm -f lex.yy.c lex.yy.cc
	rm -f a.out
	rm -f log
