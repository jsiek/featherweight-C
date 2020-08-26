a.out: syntax.l syntax.y ast.h
	bison -d syntax.y
	flex syntax.l
	bison -v --debug syntax.y
	gcc -c -g lex.yy.c
