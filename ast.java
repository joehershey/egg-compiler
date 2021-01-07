import java.io.*;
import java.util.*;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a egg program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     --------            ----
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        linked list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//       StructDeclNode    IdNode, DeclListNode
//
//     FormalsListNode     linked list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        linked list of StmtNode
//     ExpListNode         linked list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       BoolNode          -- none --
//       VoidNode          -- none --
//       StructNode        IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       RepeatStmtNode      ExpNode, DeclListNode, StmtListNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       IntLitNode          -- none --
//       StrLitNode          -- none --
//       TrueNode            -- none --
//       FalseNode           -- none --
//       IdNode              -- none --
//       DotAccessNode       ExpNode, IdNode
//       AssignNode          ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         AndNode
//         OrNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         GreaterNode
//         LessEqNode
//         GreaterEqNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of kids, or
// internal nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   BoolNode,  VoidNode,  IntLitNode,  StrLitNode,
//        TrueNode,  FalseNode, IdNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//        ProgramNode,     VarDeclNode,     FnDeclNode,     FormalDeclNode,
//        StructDeclNode,  FnBodyNode,      StructNode,     AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, ReadStmtNode,   WriteStmtNode   
//        IfStmtNode,      IfElseStmtNode,  WhileStmtNode,  CallStmtNode
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode,  CallExpNode,
//        UnaryExpNode,    BinaryExpNode,   UnaryMinusNode, NotNode,
//        PlusNode,        MinusNode,       TimesNode,      DivideNode,
//        AndNode,         OrNode,          EqualsNode,     NotEqualsNode,
//        LessNode,        GreaterNode,     LessEqNode,     GreaterEqNode
//
// **********************************************************************

// **********************************************************************
// ASTnode class (base class for all other kinds of nodes)$
// **********************************************************************

abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void addIndent(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }
}


// **********************************************************************
// ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
// StmtListNode, ExpListNode$$
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        myDeclList = L;
    }

    /**
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, struct defintions, and functions in the program.
     */
    public void nameAnalysis() {
        SymTable symTab = new SymTable();
        table = symTab;
        myDeclList.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck() {
    	
    	//check if there exists a main method in the inFile
    	//table complete because nameAnalysis was already run
    	if(table.lookupGlobal("main") == null) {
        	ErrMsg.fatal(0, 0, "No main function");
        }else {
        	
        }
    	
        myDeclList.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
    }
    
    /*
     * Code generator method cascades down the AST and
     * generates mips code
     */
    public void codeGen() {
    	myDeclList.codeGen();
    }

    // 1 kid
    private SymTable table;		//used to pass the table from name
    							//analysis to typeCheck
    private DeclListNode myDeclList;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> S) {
    	areLocals = false;
        myDecls = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        nameAnalysis(symTab, symTab);
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing struct names in variable decls), process all of the 
     * decls in the list.
     * 
     * Also helps calculate the offsets of each variable
     */    
    public void nameAnalysis(SymTable symTab, SymTable globalTab) {
    	int offset = -4; //loop starts with -4, so -4 not -8
    	int size = 0;    //keeping track of the size of the local vars
    	if(areLocals == true) {
    		for (DeclNode node : myDecls) {
                if (node instanceof VarDeclNode) {
                	//increment offset/local size
                	offset -= 4;
                	size = size + 4;
                	((VarDeclNode)node).setOffset(offset);	//method in TableSym
                    ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
                } else {
                    node.nameAnalysis(symTab);
                }
            }
    		localsSize = size; //stores size in field, so getSize() works
    	}
    	else { //no offset for globals(by default 1)
    		for (DeclNode node : myDecls) {
                if (node instanceof VarDeclNode) {
                    ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
                } else {
                    node.nameAnalysis(symTab);
                }
            }
    	}
        
    }   

    /*
     * Passes codeGen to the declarations
     */
    public void codeGen() {
    	for (DeclNode node : myDecls) {
    		if (node instanceof VarDeclNode) {
    			((VarDeclNode)node).codeGen();
    		}else if(node instanceof FnDeclNode) {
    			((FnDeclNode)node).codeGen();
    		}
        }
    }
    
    //was nameAnalysis called from within a function?
    public void areLocals() {
    	areLocals = true;
    }
    
    //Total size in bytes of the locals
    public int getSize() {
    	return localsSize;
    }
    
    /**
     * typeCheck
     */
    public void typeCheck() {
        for (DeclNode node : myDecls) {
            node.typeCheck();
        }
        
    }
    
    /*
     * Calculates the offsets of local variables nested in
     * statements. Takes the current offset as a parameter. 
     * Returns the new current offset.
     */
    public int setStatementOffsets(int offset) {
    	int off = offset - 4;
    	int size = 0;
    	for (DeclNode node : myDecls) {
            if (node instanceof VarDeclNode) {
            	size += 4;
            	off += 4;
            	off = -1 * off;		//offset is negative for locals
            	((VarDeclNode)node).setOffset(off);
            } else {
            }
            off = off * -1; 	//set back to positive
        }
    	localsSize = size; //new locals sides including nested locals
    	return off + 4;		//new current val of offset
	}
    
    public void unparse(PrintWriter p, int indent) {
        Iterator it = myDecls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }

    // list of kids (DeclNodes)
    private List<DeclNode> myDecls;
    private boolean areLocals;		//are the decls inside a function?
    private int localsSize;			//total size of the local variables
}

class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> S) {
        myFormals = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     *     
     * Also, calculates offset of the parameters and stores in TableSym
     */
    
    public List<Type> nameAnalysis(SymTable symTab) {
        List<Type> typeList = new LinkedList<Type>();
        int offset = 0;	//frame pointer
        for (FormalDeclNode node : myFormals) {
        	offset = offset + 4;	//going down the stack
        	((FormalDeclNode)node).setOffset(offset);
            TableSym sym = node.nameAnalysis(symTab);
            if (sym != null) {
                typeList.add(sym.getType());
            }
        }
        return typeList;
    }    
    
    /**
     * Return the number of formals in this list.
     */
    public int length() {
        return myFormals.size();
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = myFormals.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        myDeclList = declList;
        myStmtList = stmtList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     */
    public void nameAnalysis(SymTable symTab) {
    	myDeclList.areLocals();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
    }    
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myStmtList.typeCheck(retType);
    }    
          
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
        myStmtList.unparse(p, indent);
    }
    
    //used to get the size of the local variables, both in decls
    //and nested in statements
    public int getSize() {
    	return myDeclList.getSize() + myStmtList.getSize();
    }
    
    //pass codeGen down to the body's statements
    public void codeGen() {
    	myStmtList.codeGen();
    }
    
    /*
     * Uses local size (+ 8) as the current offset, goes
     * through statements checking for nested declarations and
     * setting offsets
     */
    public void setStatementOffsets(int localSize) {
    	myStmtList.setStatementOffsets(localSize);
    }
    
    /*
     * Passing the function exit label through to the return
     * statement
     */
    public void passExitLabel(String label) {
    	myStmtList.passExitLabel(label);
    }

    // 2 kids
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        myStmts = S;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (StmtNode node : myStmts) {
            node.nameAnalysis(symTab);
        }
    }    
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        for(StmtNode node : myStmts) {
            node.typeCheck(retType);
        }
    }
    
    
    /*
     * Returns the total size of all local variables
     * contained in conditional statements within the scope
     */
    public int getSize() {
    	int size = 0;
    	for(StmtNode node : myStmts) {
            if(node instanceof IfElseStmtNode) {
            	size += ((IfElseStmtNode)node).getSize();
            }
            else if(node instanceof IfStmtNode) {
            	size += ((IfStmtNode)node).getSize();
            }else if(node instanceof WhileStmtNode) {
            	size += ((WhileStmtNode)node).getSize();
            }
        }
    	return size;
    }
    
    /*
     * If a statement is a conditional statement, call its 
     * set offset method to check for declarations and set their
     * respective offsets
     */
    public int setStatementOffsets(int localSize) {
    	int offset = localSize;
    	for(StmtNode node : myStmts) {
            if(node instanceof IfElseStmtNode) {
            	offset = ((IfElseStmtNode)node).setStatementOffsets(offset);
            }
            else if(node instanceof IfStmtNode) {
            	offset = ((IfStmtNode)node).setStatementOffsets(offset);
            }else if(node instanceof WhileStmtNode) {
            	offset = ((WhileStmtNode)node).setStatementOffsets(offset);
            }
        }
    	return offset;	//new current offset
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = myStmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }
    }

    // list of kids (StmtNodes)
    private List<StmtNode> myStmts;

    /*
     * Generate code for each statement in the list
     */
	public void codeGen() {
		for(StmtNode node : myStmts) {
            node.codeGen();
        }
	}
	
	/*
	 * Pass the exit label from the function to the return
	 * statement. If the statement is a conditional, a return 
	 * statement may be inside, so still pass.
	 */
	public void passExitLabel(String label) {
		for(StmtNode node : myStmts) {
            if(node instanceof ReturnStmtNode) {
            	((ReturnStmtNode)node).passExitLabel(label);
            }else if(node instanceof IfStmtNode) {
            	((IfStmtNode)node).passExitLabel(label);
            }else if(node instanceof IfElseStmtNode) {
            	((IfElseStmtNode)node).passExitLabel(label);
            }else if(node instanceof WhileStmtNode) {
            	((WhileStmtNode)node).passExitLabel(label);
            }
		}
    }
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        myExps = S;
    }
    
    public int size() {
        return myExps.size();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     */
    public void nameAnalysis(SymTable symTab) {
        for (ExpNode node : myExps) {
            node.nameAnalysis(symTab);
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(List<Type> typeList) {
        int k = 0;
        try {
            for (ExpNode node : myExps) {
                Type actualType = node.typeCheck();     // actual type of arg
                
                if (!actualType.isErrorType()) {        // if this is not an error
                    Type formalType = typeList.get(k);  // get the formal type
                    if (!formalType.equals(actualType)) {
                        ErrMsg.fatal(node.lineNum(), node.charNum(),
                                     "Type of actual does not match type of formal");
                    }
                }
                k++;
            }
        } catch (NoSuchElementException e) {
            System.err.println("unexpected NoSuchElementException in ExpListNode.typeCheck");
            System.exit(-1);
        }
    }
    
    /*
     * continue generating code for each expression
     */
    public void codeGen() {
    	for (ExpNode node : myExps) {
            node.codeGen();
        }
	}
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = myExps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (ExpNodes)
    private List<ExpNode> myExps;

}

// **********************************************************************
// DeclNode and its subclasses$
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /**
     * Note: a formal decl needs to return a sym
     */
    abstract public TableSym nameAnalysis(SymTable symTab);

    // default version of typeCheck for non-function decls
    public void typeCheck() { }
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        myType = type;
        myId = id;
        mySize = size;
    }

    /**
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a struct type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for struct field decls)
     * globalTab is global symbol table (for struct type names)
     * symTab and globalTab can be the same
     */
    public TableSym nameAnalysis(SymTable symTab) {
        return nameAnalysis(symTab, symTab);
    }
    
    public TableSym nameAnalysis(SymTable symTab, SymTable globalTab) {
        boolean badDecl = false;
        String name = myId.name();
        TableSym sym = null;
        IdNode structId = null;

        if (myType instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (myType instanceof StructNode) {
            structId = ((StructNode)myType).idNode();
            sym = globalTab.lookupGlobal(structId.name());
            
            // if the name for the struct type is not found, 
            // or is not a struct type
            if (sym == null || !(sym instanceof StructDefSym)) {
                ErrMsg.fatal(structId.lineNum(), structId.charNum(), 
                             "Invalid name of struct type");
                badDecl = true;
            }
            else {
                structId.link(sym);
            }
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (myType instanceof StructNode) {
                    sym = new StructSym(structId);
                }
                else {
                    sym = new TableSym(myType.type(), offset); //set the sym with an offset
                    ts = sym;	//save the symbol for later offset manipulation
                }
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
                System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } 
        }
        
        
        return sym;
    }    
    
    /*
     * Sets the offset for variables nested in statements that 
     * did not get proper offsets in the first pass
     */
    public void setOffset(int offset) {
    	this.offset = offset;
    	if(ts != null) {	
    		ts.setOffset(offset);
    	}
    	
    }
    
    /*
     * generates the code for global variables (offset == 1)
     */
    public void codeGen() {
    	if(offset == 1) {
    		Codegen.generate(".data");
    		Codegen.generateWithComment(".align 2", "Align on word boundaries");
    		Codegen.generateLabeled("__" + myId.name(), ".space 4", "4 bytes space");
    	}
    }
    
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.println(";");
    }

    // 3 kids
    private TypeNode myType;
    private int offset = 1;		//initially set to one, overwritten by locals
    private IdNode myId;
    private TableSym ts;
    private int mySize;  // use value NOT_STRUCT if this is not a struct type

    public static int NOT_STRUCT = -1;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        myType = type;
        myId = id;
        myFormalsList = formalList;
        myBody = body;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     */
    public TableSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        FnSym sym = null;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(),
                         "Multiply declared identifier");
        }
        
        else { // add function name to local symbol table
            try {
                sym = new FnSym(myType.type(), myFormalsList.length());
                paramSize = myFormalsList.length() * 4;
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
                System.err.println("Unexpected WrongArgumentException " +
                                   " in FnDeclNode.nameAnalysis");
                System.exit(-1);
            } 
        }
        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = myFormalsList.nameAnalysis(symTab);
        if (sym != null) {
            sym.addFormals(typeList);
        }
        
        myBody.nameAnalysis(symTab); // process the function body
        localSize = myBody.getSize(); //find the size of all un-nested locals for offset
        myBody.setStatementOffsets(localSize + 8); //current locals + 8 = current offset
        localSize = myBody.getSize(); //size after nested locals are counted
        
        try {
            symTab.removeScope();  // exit scope
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        return null;
    } 
       
    /**
     * typeCheck
     */
    public void typeCheck() {
        myBody.typeCheck(myType.type());
    }
    
    /*
     * generates code for a given method. First tackles prelude,
     * then the body is called with codeGen(), which returns to to the
     * exit label and completes the epilogue
     */
    public void codeGen() {
    	Codegen.generate(".text");
    	if(myId.name().equalsIgnoreCase("main")) {
    		Codegen.generate(".globl main");
    	}
    	
    	Codegen.genLabel("__" + myId.name(), "METHOD ENTRY");
    	Codegen.genPush(Codegen.RA);
    	Codegen.genPush(Codegen.FP);
    	Codegen.generate("subu", Codegen.SP, Codegen.SP, localSize);
    	Codegen.generate("addu", Codegen.FP, Codegen.SP, 8 + localSize);
    	
    	Codegen.generateWithComment("", "Func body");
    	
    	String exitLabel = Codegen.nextLabel();	//must know the exit label
    	myBody.passExitLabel(exitLabel);	//pass to the return statement for jump
    	
    	myBody.codeGen();	//generate body code
    	
    	Codegen.genLabel(exitLabel, "Function Exit");	//return stmt jumps here
    	
    	//epilogue
    	Codegen.generateIndexed("lw", Codegen.RA, Codegen.FP, 0);
    	Codegen.generate("move", Codegen.T0, Codegen.FP);
    	Codegen.generateIndexed("lw", Codegen.FP, Codegen.FP, -4);
    	Codegen.generate("move", Codegen.SP, Codegen.T0);
    	Codegen.generate("jr", Codegen.RA);
    	
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.print("(");
        myFormalsList.unparse(p, 0);
        p.println(") {");
        myBody.unparse(p, indent+4);
        p.println("}\n");
    }

    // 4 kids
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
    private int paramSize;
    private int localSize;	
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        myType = type;
        myId = id;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     * 
     * Also sets the offset of each parameter
     */
    public TableSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        TableSym sym = null;
        
        if (myType instanceof VoidNode) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;
        }
        
        if (!badDecl) {  // insert into symbol table
            try {
                sym = new TableSym(myType.type(), offset); //set param offset
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
                System.err.println("Unexpected WrongArgumentException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }        
	}
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
    }
    
    /*
     * Used so nameAnalysis can access the offset, which comes
     * from formalsListNode
     */
    public void setOffset(int offset) {
    	this.offset = offset;
    }

    // 2 kids
    private int offset;
    private TypeNode myType;
    private IdNode myId;
}

class StructDeclNode extends DeclNode {
    public StructDeclNode(IdNode id, DeclListNode declList) {
        myId = id;
        myDeclList = declList;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this struct definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this struct
     */
    public TableSym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        
        if (symTab.lookupLocal(name) != null) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Multiply declared identifier");
            badDecl = true;            
        }

        SymTable structSymTab = new SymTable();
        
        // process the fields of the struct
        myDeclList.nameAnalysis(structSymTab, symTab);
        
        if (!badDecl) {
            try {   // add entry to symbol table
                StructDefSym sym = new StructDefSym(structSymTab);
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (WrongArgumentException ex) {
                System.err.println("Unexpected WrongArgumentException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } 
        }
        
        return null;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("struct ");
        p.print(myId.name());
        p.println("{");
        myDeclList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("};\n");

    }

    // 2 kids
    private IdNode myId;
    private DeclListNode myDeclList;
}

// **********************************************************************
// TypeNode and its Subclasses$$
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method */
    abstract public Type type();
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("int");
    }
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /**
     * type
     */
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("bool");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /**
     * type
     */
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class StructNode extends TypeNode {
    public StructNode(IdNode id) {
        myId = id;
    }

    public IdNode idNode() {
        return myId;
    }
    
    /**
     * type
     */
    public Type type() {
        return new StructType(myId);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("struct ");
        p.print(myId.name());
    }
    
    // 1 kid
    private IdNode myId;
}

// **********************************************************************
// StmtNode and its subclasses$
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void nameAnalysis(SymTable symTab);
    protected abstract void codeGen();
	abstract public void typeCheck(Type retType);
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignNode assign) {
        myAssign = assign;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myAssign.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myAssign.typeCheck();
    }
    
    /*
     * Pass code generation to the assign expression
     */
    public void codeGen(Type retType) {
        myAssign.codeGen();
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myAssign.unparse(p, -1); // no parentheses
        p.println(";");
    }
    
    /*
     * Pass code generation to the assign expression
     */
    public void codeGen() {
    	myAssign.codeGen();
    }

    // 1 kid
    private AssignNode myAssign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("++;");
    }
    
    /*
     * Increment variable with registers and store in RAM
     */
    public void codeGen() {
    	myExp.codeGen();
    	((IdNode)myExp).genAddr();
    	Codegen.genPop(Codegen.T1);		//pop var address
    	Codegen.genPop(Codegen.T0);		//pop expression	
    	Codegen.generate("addi", Codegen.T0, Codegen.T0, 1);
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);
    }

    // 1 kid
    private ExpNode myExp;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("--;");
    }
    
    /*
     * Decrement variable with registers and store in RAM
     */
    public void codeGen() {
    	myExp.codeGen();
    	((IdNode)myExp).genAddr();
    	Codegen.genPop(Codegen.T1);		//pop var address
    	Codegen.genPop(Codegen.T0);		//pop expression	
    	Codegen.generate("addi", Codegen.T0, Codegen.T0, -1);
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.T1, 0);
    }
    
    // 1 kid
    private ExpNode myExp;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        myExp = e;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }    
 
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to read a struct variable");
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("cin >> ");
        myExp.unparse(p, 0);
        p.println(";");
    }
    
    public void codeGen() {
    	((IdNode)myExp).genAddr();		//push address to stack
    	Codegen.genPop(Codegen.T0);		//pop address into t0
    	//system reads input
    	Codegen.generate("li", Codegen.V0, 5);
    	Codegen.generate("syscall");
    	Codegen.generateIndexed("sw", Codegen.V0, Codegen.T0, 0); //store at address
    }

    // 1 kid (actually can only be an IdNode or an ArrayExpNode)
    private ExpNode myExp;
}

class WriteStmtNode extends StmtNode {
    public WriteStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write a struct variable");
        }
        
        if (type.isVoidType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Attempt to write void");
        }
    }
    
    /*
     * Generates code for a write statement, which varies by
     * type
     */
    public void codeGen() {
    	Type type = myExp.typeCheck();
    	if (type.isIntType()) {
    		myExp.codeGen();
        	Codegen.genPop(Codegen.A0);
        	Codegen.generate("li", Codegen.V0, 1);	//syscall for integers
        	Codegen.generate("syscall");
    	}
    	else if (type.isStringType()) {
    		myExp.codeGen();
        	Codegen.genPop(Codegen.A0);
        	Codegen.generate("li", Codegen.V0, 4);  //syscall for strings
        	Codegen.generate("syscall");
    	}
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("cout << ");
        myExp.unparse(p, 0);
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myDeclList = dlist;
        myExp = exp;
        myStmtList = slist;
    }
    
    /*
     * Decls could be within an if statement, so this method
     * is attemting to give those decls an offset
     */
    public int setStatementOffsets(int offset) {
    	offset = myDeclList.setStatementOffsets(offset);
    	offset = myStmtList.setStatementOffsets(offset); //more conditionals?
    	return offset;
	}

	/**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
     /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
    
    /*
     * Locals size should add the size of locals in the if nest,
     * and those within its statements
     */
    public int getSize() {
    	return myDeclList.getSize() + myStmtList.getSize();
    }
    
    public void codeGen() {
    	String fLabel = Codegen.nextLabel(); //false label
    	myExp.codeGen();
    	Codegen.genPop(Codegen.T0);
    	Codegen.generateWithComment("beq", "branch if false", Codegen.T0, Codegen.FALSE, fLabel);
    	myStmtList.codeGen();
    	Codegen.generateWithComment("j", "end true branch", fLabel);
    	Codegen.genLabel(fLabel, "successer"); //end of statement
    	
    }
    
    /*
     * Passing exit label to the return statement
     */
    public void passExitLabel(String label) {
    	myStmtList.passExitLabel(label);
    }
       
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("}");
    }

    // e kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
                          StmtListNode slist1, DeclListNode dlist2,
                          StmtListNode slist2) {
        myExp = exp;
        myThenDeclList = dlist1;
        myThenStmtList = slist1;
        myElseDeclList = dlist2;
        myElseStmtList = slist2;
    }
    
    /*
     * Decls could be within an if statement, so this method
     * is attemting to give those decls an offset.
     * 
     * Must be done for both blocks, including statements
     */
    public int setStatementOffsets(int offset) {
    	offset = myThenDeclList.setStatementOffsets(offset);
    	offset = myThenStmtList.setStatementOffsets(offset);
    	offset = myElseDeclList.setStatementOffsets(offset);
    	offset = myElseStmtList.setStatementOffsets(offset);
    	return offset;
	}

	/**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myThenDeclList.nameAnalysis(symTab);
        myThenStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        myElseDeclList.nameAnalysis(symTab);
        myElseStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as an if condition");        
        }
        
        myThenStmtList.typeCheck(retType);
        myElseStmtList.typeCheck(retType);
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myThenDeclList.unparse(p, indent+4);
        myThenStmtList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("}");
        addIndent(p, indent);
        p.println("else {");
        myElseDeclList.unparse(p, indent+4);
        myElseStmtList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("}");        
    }
    
    /*
     * Locals size should add the size of locals in the if nest,
     * and those within its statements
     */
    public int getSize() {
    	return myThenDeclList.getSize() + myElseStmtList.getSize() +
    			myElseDeclList.getSize() + myThenStmtList.getSize();
    }
    
    public void codeGen() {
    	String eLabel = Codegen.nextLabel();	//else label
    	String sLabel = Codegen.nextLabel();	//successor label
    	myExp.codeGen();
    	Codegen.genPop(Codegen.T0);
    	Codegen.generateWithComment("beq", "branch to else if false", Codegen.T0, Codegen.FALSE, eLabel);
    	myThenStmtList.codeGen();
    	Codegen.generateWithComment("j", "end then branch", sLabel);
    	Codegen.genLabel(eLabel, "else branch");
    	myElseStmtList.codeGen();
    	Codegen.generateWithComment("j", "end else branch", sLabel);
    	Codegen.genLabel(sLabel, "successor");
    }
    
    /*
     * Passing exit label to the return statement
     */
    public void passExitLabel(String label) {
    	myThenStmtList.passExitLabel(label);
    	myElseStmtList.passExitLabel(label);
    }

    // 5 kids
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }
    
    
    /*
     * Decls could be within a while statement, so this method
     * is attemting to give those decls an offset.
     */
    public int setStatementOffsets(int offset) {
    	offset = myDeclList.setStatementOffsets(offset);
    	offset = myStmtList.setStatementOffsets(offset);
    	return offset;
	}

	/**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression used as a while condition");        
        }
        
        myStmtList.typeCheck(retType);
    }
    
    /*
     * Locals size should add the size of locals in the while loop,
     * and those within its statements
     */
    public int getSize() {
    	return myDeclList.getSize() + myStmtList.getSize();
    }
    
    public void codeGen() {
    	String exitLabel = Codegen.nextLabel(); //if condition is false
    	String loopLabel = Codegen.nextLabel();	//where condition is tested
    	Codegen.genLabel(loopLabel, "loop head");
    	myExp.codeGen();
    	Codegen.genPop(Codegen.T0);
    	Codegen.generateWithComment("beq", "branch if false", Codegen.T0, Codegen.FALSE, exitLabel);
    	myStmtList.codeGen();
    	Codegen.generateWithComment("j", "go to loop head", loopLabel);
    	Codegen.genLabel(exitLabel, "loop successor");
    	
    }
    
    //Pass to return statement for jumping
    public void passExitLabel(String label) {
    	myStmtList.passExitLabel(label);
    	
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("while (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("}");
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class RepeatStmtNode extends StmtNode {
    public RepeatStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab);
        myStmtList.nameAnalysis(symTab);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-integer expression used as a repeat clause");        
        }
        
        myStmtList.typeCheck(retType);
    }
    
    public int getSize() {
    	return myDeclList.getSize();
    }
        
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("repeat (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        addIndent(p, indent);
        p.println("}");
    }
    
    public void codeGen() {
    	
    }

    // 3 kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}


class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        myCall = call;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myCall.nameAnalysis(symTab);
    }
    
    /*
     * generate code from the call expression
     */
    public void codeGen() {
    	myCall.codeGen();
    }
    
    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        myCall.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        myCall.unparse(p, indent);
        p.println(";");
    }

    // 1 kid
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     */
    public void nameAnalysis(SymTable symTab) {
        if (myExp != null) {
            myExp.nameAnalysis(symTab);
        }
    }
    
    /*
     * Calculate return value, pop into v0 for returning
     * Jump to exit label
     */
    public void codeGen() {
    	if(myExp != null) {
    		myExp.codeGen();
    		Codegen.genPop(Codegen.V0);
    		Codegen.generate("j", exitLabel);
    	}else {
    		Codegen.generate("j", exitLabel);
    	}
    }
    
    /*
     * The exit label has arrived at its destination, used
     * to jump to the function epilogue
     */
    public void passExitLabel(String label) {
		exitLabel = label;
    }

    /**
     * typeCheck
     */
    public void typeCheck(Type retType) {
        if (myExp != null) {  // return value given
            Type type = myExp.typeCheck();
            
            if (retType.isVoidType()) {
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Return with a value in a void function");                
            }
            
            else if (!retType.isErrorType() && !type.isErrorType() && !retType.equals(type)){
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Bad return value");
            }
        }
        
        else {  // no return value given -- ok if this is a void function
            if (!retType.isVoidType()) {
                ErrMsg.fatal(0, 0, "Missing return value");                
            }
        }
        
    }
    
    public void unparse(PrintWriter p, int indent) {
        addIndent(p, indent);
        p.print("return");
        if (myExp != null) {
            p.print(" ");
            myExp.unparse(p, 0);
        }
        p.println(";");
    }

    // 1 kid
    private ExpNode myExp; // possibly null
    private String exitLabel;	//where to jump
}

// **********************************************************************
// ExpNode and its subclasses$$
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /**
     * Default version for nodes with no names
     */
    public void nameAnalysis(SymTable symTab) { }
    
    abstract public Type typeCheck();
    abstract public int lineNum();
    abstract public int charNum();

	protected abstract void codeGen();
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myIntVal = intVal;
    }
    
    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
        
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new IntType();
    }
    
    /*
     * push the value on the stack
     */
    public void codeGen() {
    	Codegen.generate("li", Codegen.T0, myIntVal); //immediate val
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.SP, 0);
    	Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print(myIntVal);
    }

    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }
    
    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new StringType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
    }
    
    /*
     * Stores string lit in data segment, and pushes it onto the stack
     */
    public void codeGen() {
    	String label = Codegen.nextLabel();
    	Codegen.generate(".data");
    	Codegen.generateLabeled(label, ".asciiz", "String", myStrVal);
    	Codegen.generate(".text");
    	Codegen.generate("la", Codegen.T0, label);
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.SP, 0);
    	Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
    	
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
    
    /*
     * Push 1/TRUE to the stack 
     */
    public void codeGen() {
    	Codegen.generate("li", Codegen.T0, Codegen.TRUE);
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.SP, 0);
    	Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    private int myLineNum;
    private int myCharNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /**
     * Return the line number for this literal.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this literal.
     */
    public int charNum() {
        return myCharNum;
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        return new BoolType();
    }
    
    /*
     * Push 0/FALSE to the stack 
     */
    public void codeGen() {
    	Codegen.generate("li", Codegen.T0, Codegen.FALSE);
    	Codegen.generateIndexed("sw", Codegen.T0, Codegen.SP, 0);
    	Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }

    private int myLineNum;
    private int myCharNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }

    /**
     * Link the given symbol to this ID.
     */
    public void link(TableSym sym) {
        mySym = sym;
    }
    
    /**
     * Return the name of this ID.
     */
    public String name() {
        return myStrVal;
    }
    
    /**
     * Return the symbol associated with this ID.
     */
    public TableSym sym() {
        return mySym;
    }
    
    /**
     * Return the line number for this ID.
     */
    public int lineNum() {
        return myLineNum;
    }
    
    /**
     * Return the char number for this ID.
     */
    public int charNum() {
        return myCharNum;
    }    
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     */
    public void nameAnalysis(SymTable symTab) {
        TableSym sym = symTab.lookupGlobal(myStrVal);
        
        if (sym == null) {
            ErrMsg.fatal(myLineNum, myCharNum, "Undeclared identifier");
        } else {
            link(sym);
        }
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (mySym != null) {
            return mySym.getType();
        } 
        else {
            System.err.println("ID with null sym field in IdNode.typeCheck");
            System.exit(-1);
        }
        return null;
    }
    
    /*
     * Generated when a function call is executed
     */
    public void genJumpAndLink(){
    	if(myStrVal == "main") {
    		Codegen.generate("jal", "main");
    	}else {
    		Codegen.generate("jal", "__" + myStrVal);
    	}
    }
    
    /*
     * Generated to get a var value and push it onto the stack
     */
    public void codeGen(){
    	//for global variables
    	if(mySym.getOffset() == 1) {
    		Codegen.generate("lw", Codegen.T0, "__" + myStrVal);
    		Codegen.genPush(Codegen.T0);
    	}else {//local
    		Codegen.generate("lw", Codegen.T0, Codegen.FP, mySym.getOffset());
    		Codegen.genPush(Codegen.T0);
    	}
    }

    /*
     * Generated to get a var address and push it onto the stack
     */
    public void genAddr(){
    	//for global variables
    	if(mySym.getOffset() == 1) {
    		Codegen.generate("la", Codegen.T0, "__" + myStrVal);
    		Codegen.genPush(Codegen.T0);
    	}else {//local
    		Codegen.generate("la", Codegen.T0, Codegen.FP, mySym.getOffset());
    		Codegen.genPush(Codegen.T0);
    	}
    }
           
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
        if (mySym != null) {
            p.print("(" + mySym + ")");
        }
    }

    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private TableSym mySym;
}

class DotAccessExpNode extends ExpNode {
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        myLoc = loc;    
        myId = id;
        mySym = null;
    }

    /**
     * Return the symbol associated with this dot-access node.
     */
    public TableSym sym() {
        return mySym;
    }    
    
    /**
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a struct type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate struct definition
     */
    public void nameAnalysis(SymTable symTab) {
        badAccess = false;
        SymTable structSymTab = null; // to lookup RHS of dot-access
        TableSym sym = null;
        
        myLoc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if myLoc is really an ID, then sym will be a link to the ID's symbol
        if (myLoc instanceof IdNode) {
            IdNode id = (IdNode)myLoc;
            sym = id.sym();
            
            // check ID has been declared to be of a struct type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof StructSym) { 
                // get symbol table for struct type
                TableSym tempSym = ((StructSym)sym).getStructType().sym();
                structSymTab = ((StructDefSym)tempSym).getSymTable();
            } 
            else {  // LHS is not a struct type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-struct type");
                badAccess = true;
            }
        }
        
        // if myLoc is really a dot-access (i.e., myLoc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a struct type, or
        // a link to the TableSym for the struct type RHSid was declared to be
        else if (myLoc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode)myLoc;
            
            if (loc.badAccess) {  // if errors in processing myLoc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing myLoc
                sym = loc.sym();

                if (sym == null) {  // no struct in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-struct type");
                    badAccess = true;
                }
                else {  // get the struct's symbol table in which to lookup RHS
                    if (sym instanceof StructDefSym) {
                        structSymTab = ((StructDefSym)sym).getSymTable();
                    }
                    else {
                        System.err.println("Unexpected TableSym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing myLoc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the struct's symbol table
        if (!badAccess) {
        
            sym = structSymTab.lookupGlobal(myId.name()); // lookup
            if (sym == null) { // not found - RHS is not a valid field name
                ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                             "Invalid struct field name");
                badAccess = true;
            }
            
            else {
                myId.link(sym);  // link the symbol
                // if RHS is itself as struct type, link the symbol for its struct 
                // type to this dot-access node (to allow chained dot-access)
                if (sym instanceof StructSym) {
                    mySym = ((StructSym)sym).getStructType().sym();
                }
            }
        }
    }    
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        return myId.typeCheck();
    }
    
    public void codeGen() {
    	//do nothing
    }
    
    public void unparse(PrintWriter p, int indent) {
        myLoc.unparse(p, 0);
        p.print(".");
        myId.unparse(p, 0);
    }

    // 2 kids
    private ExpNode myLoc;    
    private IdNode myId;
    private TableSym mySym;          // link to TableSym for struct type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignNode extends ExpNode {
    public AssignNode(ExpNode lhs, ExpNode exp) {
        myLhs = lhs;
        myExp = exp;
    }
    
    /**
     * Return the line number for this assignment node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myLhs.lineNum();
    }
    
    /**
     * Return the char number for this assignment node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myLhs.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myLhs.nameAnalysis(symTab);
        myExp.nameAnalysis(symTab);
    }
 
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type typeLhs = myLhs.typeCheck();
        Type typeExp = myExp.typeCheck();
        Type retType = typeLhs;
        
        if (typeLhs.isFnType() && typeExp.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Function assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructDefType() && typeExp.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct name assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructType() && typeExp.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct variable assignment");
            retType = new ErrorType();
        }        
        
        if (!typeLhs.equals(typeExp) && !typeLhs.isErrorType() && !typeExp.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Type mismatch");
            retType = new ErrorType();
        }
        
        if (typeLhs.isErrorType() || typeExp.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
    
    /*
     * Stores value of RHS in the address specified by the LHS/loc
     * 
     */
    public void codeGen() {
    	myExp.codeGen();
    	((IdNode)myLhs).genAddr();
    	Codegen.genPop(Codegen.T0);  //LOC of LHS id --> t0
    	Codegen.genPop(Codegen.T1);  //Val of RHS --> t1
    	Codegen.generateIndexed("sw", Codegen.T1, Codegen.T0, 0);	//store
    	Codegen.genPush(Codegen.T1);	//a copy is pushed onto the stack
    }
    
    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        myLhs.unparse(p, 0);
        p.print(" = ");
        myExp.unparse(p, 0);
        if (indent != -1)  p.print(")");
    }

    // 2 kids
    private ExpNode myLhs;
    private ExpNode myExp;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        myId = name;
        myExpList = elist;
    }

    public CallExpNode(IdNode name) {
        myId = name;
        myExpList = new ExpListNode(new LinkedList<ExpNode>());
    }

    /**
     * Return the line number for this call node. 
     * The line number is the one corresponding to the function name.
     */
    public int lineNum() {
        return myId.lineNum();
    }
    
    /**
     * Return the char number for this call node.
     * The char number is the one corresponding to the function name.
     */
    public int charNum() {
        return myId.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myId.nameAnalysis(symTab);
        myExpList.nameAnalysis(symTab);
    }  
      
    /**
     * typeCheck
     */
    public Type typeCheck() {
        if (!myId.typeCheck().isFnType()) {  
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Attempt to call a non-function");
            return new ErrorType();
        }
        
        FnSym fnSym = (FnSym)(myId.sym());
        
        if (fnSym == null) {
            System.err.println("null sym for Id in CallExpNode.typeCheck");
            System.exit(-1);
        }
        
        if (myExpList.size() != fnSym.getNumParams()) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Function call with wrong number of args");
            return fnSym.getReturnType();
        }
        
        myExpList.typeCheck(fnSym.getParamTypes());
        return fnSym.getReturnType();
    }
        
    // ** unparse **
    public void unparse(PrintWriter p, int indent) {
        myId.unparse(p, 0);
        p.print("(");
        if (myExpList != null) {
            myExpList.unparse(p, 0);
        }
        p.print(")");
    }
    
    /*
     * Push args onto the stack, execute the callee and save the 
     * return value at v0, then push that onto the stack for use.
     */
    public void codeGen() {
    	myExpList.codeGen();
    	myId.genJumpAndLink();
    	Codegen.genPush(Codegen.V0);
    	if(((FnSym)myId.sym()).getReturnType().isVoidType()) {
    		Codegen.genPop(Codegen.T0);	//no value should be pushed
    									//for void type fn calls
    	}
    	
    }

    // 2 kids
    private IdNode myId;
    private ExpListNode myExpList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        myExp = exp;
    }
    
    /**
     * Return the line number for this unary expression node. 
     * The line number is the one corresponding to the  operand.
     */
    public int lineNum() {
        return myExp.lineNum();
    }
    
    /**
     * Return the char number for this unary expression node.
     * The char number is the one corresponding to the  operand.
     */
    public int charNum() {
        return myExp.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     */
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    // one child
    protected ExpNode myExp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        myExp1 = exp1;
        myExp2 = exp2;
    }
    
    /**
     * Return the line number for this binary expression node. 
     * The line number is the one corresponding to the left operand.
     */
    public int lineNum() {
        return myExp1.lineNum();
    }
    
    /**
     * Return the char number for this binary expression node.
     * The char number is the one corresponding to the left operand.
     */
    public int charNum() {
        return myExp1.charNum();
    }
    
    /**
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     */
    public void nameAnalysis(SymTable symTab) {
        myExp1.nameAnalysis(symTab);
        myExp2.nameAnalysis(symTab);
    }
    
    // two kids
    protected ExpNode myExp1;
    protected ExpNode myExp2;
}

// **********************************************************************
// Subclasses of UnaryExpNode$
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new IntType();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        myExp.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Evaluate the expression, then push the negative value of it
     * to the stack
     */
    public void codeGen() {
    	myExp.codeGen();
    	Codegen.genPop(Codegen.T0);
    	Codegen.generate("li", Codegen.T1, 0); 
    	Codegen.generate("sub", Codegen.T0, Codegen.T1, Codegen.T0); //0 - num = -num
    	Codegen.genPush(Codegen.T0);
    	
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }

    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new BoolType();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(!");
        myExp.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Evaluate the expression, then push the notted value of it
     * to the stack
     */
    public void codeGen() {
    	myExp.codeGen();
    	Codegen.genPop(Codegen.T0);
    	Codegen.generate("not", Codegen.T0, Codegen.T0);
    	Codegen.genPush(Codegen.T0);
    }
}

// **********************************************************************
// Subclasses of BinaryExpNode$$
// **********************************************************************

abstract class ArithmeticExpNode extends BinaryExpNode {
    public ArithmeticExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new IntType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Arithmetic operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class LogicalExpNode extends BinaryExpNode {
    public LogicalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isBoolType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isBoolType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Logical operator applied to non-bool operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class EqualityExpNode extends BinaryExpNode {
    public EqualityExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (type1.isVoidType() && type2.isVoidType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to void functions");
            retType = new ErrorType();
        }
        
        if (type1.isFnType() && type2.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to functions");
            retType = new ErrorType();
        }
        
        if (type1.isStructDefType() && type2.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct names");
            retType = new ErrorType();
        }
        
        if (type1.isStructType() && type2.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator applied to struct variables");
            retType = new ErrorType();
        }        
        
        if (!type1.equals(type2) && !type1.isErrorType() && !type2.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Type mismatch");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class RelationalExpNode extends BinaryExpNode {
    public RelationalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /**
     * typeCheck
     */
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Relational operator applied to non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

class PlusNode extends ArithmeticExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" + ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, and push the
     * value of their sum
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("add", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class MinusNode extends ArithmeticExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" - ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, and push the
     * value of their difference
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("sub", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class TimesNode extends ArithmeticExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" * ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, and push the
     * value of their product
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("mul", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class DivideNode extends ArithmeticExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" / ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, and push the
     * value of their quotient
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("div", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class AndNode extends LogicalExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" && ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, and push complete an
     * and calculation to push their boolean value
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("and", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class OrNode extends LogicalExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" || ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if either
     * side equates to true, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("or", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class EqualsNode extends EqualityExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" == ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the expressions
     * are equal, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("seq", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class NotEqualsNode extends EqualityExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" != ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the expressions
     * are not equal, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("snq", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class LessNode extends RelationalExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" < ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the LHS is less
     * than the value of the RHS, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("slt", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class GreaterNode extends RelationalExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" > ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the LHS is greater
     * than the value of the RHS, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("sgt", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class LessEqNode extends RelationalExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" <= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the LHS is less
     * than or equal to the value of the RHS, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("sle", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}

class GreaterEqNode extends RelationalExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" >= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    
    /*
     * Generate code for each side, pop their values, push TRUE if the LHS is greater
     * than the value of the RHS, else FALSE
     */
    public void codeGen() {
    	myExp1.codeGen();
    	myExp2.codeGen();
    	Codegen.genPop(Codegen.T1);		//pop rhs
    	Codegen.genPop(Codegen.T0);		//pop lhs
    	Codegen.generate("sge", Codegen.T0, Codegen.T0, Codegen.T1);
    	Codegen.genPush(Codegen.T0);
    }
}
