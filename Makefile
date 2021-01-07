###
#
# make clean removes all generated files.
#
###

JC = javac

CP = ./deps:.

P6.class: P6.java parser.class Yylex.class ASTnode.class
	$(JC) -g -cp $(CP) P6.java

parser.class: parser.java ASTnode.class Yylex.class ErrMsg.class
	$(JC) -g -cp $(CP) parser.java

parser.java: egg.cup
	java -cp $(CP) java_cup.Main < egg.cup

Yylex.class: egg.jlex.java sym.class ErrMsg.class
	$(JC) -g -cp $(CP) egg.jlex.java

ASTnode.class: ast.java Type.java TableSym.class
	$(JC) -g -cp $(CP) ast.java Type.java

egg.jlex.java: egg.jlex sym.class
	java -cp $(CP) JLex.Main egg.jlex

sym.class: sym.java
	$(JC) -g -cp $(CP) sym.java

sym.java: egg.cup
	java java_cup.Main < egg.cup

ErrMsg.class: ErrMsg.java
	$(JC) -g -cp $(CP) ErrMsg.java

TableSym.class: TableSym.java Type.class ast.java
	$(JC) -g -cp $(CP) TableSym.java ast.java

SymTable.class: SymTable.java TableSym.class DuplicateSymException.class EmptySymTableException.class WrongArgumentException.class
	$(JC) -g -cp $(CP) SymTable.java

Type.class: Type.java ast.java TableSym.java
	$(JC) -g -cp $(CP) Type.java ast.java TableSym.java

WrongArgumentException.class: WrongArgumentException.java
	$(JC) -g -cp $(CP) WrongArgumentException.java

DuplicateSymException.class: DuplicateSymException.java
	$(JC) -g -cp $(CP) DuplicateSymException.java

EmptySymTableException.class: EmptySymTableException.java
	$(JC) -g -cp $(CP) EmptySymTableException.java

###
# test
#
test:
	java -cp $(CP) P6 test.egg test.s

###
# clean
###
clean:
	rm -f *~ *.class parser.java egg.jlex.java sym.java

cleantest:
	rm -f test.s
