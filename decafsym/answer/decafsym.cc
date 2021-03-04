
#include "default-defs.h"
#include <list>
#include <map>
#include <ostream>
#include <iostream>
#include <sstream>
#include <string>
#ifndef YYTOKENTYPE
#include "decafsym.tab.h"
#endif

using namespace std;
/// definiation for symbol table

struct descriptor {
	int type;
	int lineo;
	string name;
};

typedef map<string, descriptor* > symbol_table;

typedef list<symbol_table > symbol_table_list;
symbol_table_list symtbl;

/// decafAST - Base class for all abstract syntax tree nodes.


enum decafBinaryOP {Plus , Minus , Mult , Div , Leftshift , Rightshift , Mod , Lt , Gt , Leq , Geq , Eq , Neq , And , Or};

class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
};

string getString(decafAST *d) {
	if (d != NULL) {
		return d->str();
	} else {
		return string("None");
	}
}

template <class T>
string commaList(list<T> vec) {
    string s("");
    for (typename list<T>::iterator i = vec.begin(); i != vec.end(); i++) { 
        s = s + (s.empty() ? string("") : string(",")) + (*i)->str(); 
    }   
    if (s.empty()) {
        s = string("None");
    }   
    return s;
}

/// decafStmtList - List of Decaf statements
class decafStmtList : public decafAST {
	list<decafAST *> stmts;
public:
	decafStmtList() {}
	~decafStmtList() {
		for (list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) { 
			delete *i;
		}
	}
	int size() { return stmts.size(); }
	void push_front(decafAST *e) { stmts.push_front(e); }
	void push_back(decafAST *e) { stmts.push_back(e); }
	void move_to(decafStmtList * target){
		assert( target != NULL );
		for( list<decafAST *>::iterator i = stmts.begin(); i != stmts.end(); i++) {
			target->push_back( *i );
		}
		stmts.clear();
	}
	string str() { return commaList<class decafAST *>(stmts); }
};

// package = Package(identifier name, field_decl* field_list, method_decl* method_list)
class PackageAST : public decafAST {
	string Name;
	decafStmtList *FieldDeclList;
	decafStmtList *MethodDeclList;
public:
	PackageAST(string name, decafStmtList *fieldlist, decafStmtList *methodlist) 
		: Name(name), FieldDeclList(fieldlist), MethodDeclList(methodlist) {}
	~PackageAST() { 
		if (FieldDeclList != NULL) { delete FieldDeclList; }
		if (MethodDeclList != NULL) { delete MethodDeclList; }
	}
	string str() { 
		return string("Package") + "(" + Name + "," + getString(FieldDeclList) + "," + getString(MethodDeclList) + ")";
	}
};

/// ProgramAST - the decaf program
// prog = Program(extern* extern_list, package body)
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() { return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }
};

class TypeAST : public decafAST {
	string * Name;

public:
	TypeAST(string * name): Name(name) {};
	~TypeAST() { if(Name) delete Name; };
	string str() {
		return string(*Name);
	}	
};

class VarDefAST : public decafAST {
	string Name;
	decafStmtList * TypeList;

	public:
		VarDefAST(decafStmtList * typelist): TypeList(typelist) {}
		VarDefAST(std::string name, TypeAST * type ) {
			decafStmtList * typelist = new decafStmtList();
			typelist->push_back(type);
			TypeList = typelist;
			Name = name;
		};	

		~VarDefAST() {
			if( TypeList ) delete TypeList;
		}


		string str(){
			if (TypeList->size() == 0)
				return string("None");
			else if(TypeList->size() == 1 && Name != "")
				return string("VarDef(") + Name + "," + getString(TypeList) + ")";
			else
				return string("VarDef(") + getString(TypeList) + ")";
		}
};




// ExternFunction(identifier name, method_type return_type, extern_type* typelist)
class ExternFunctionAST : public decafAST {
	string Name;
	TypeAST * ReturnType;
	VarDefAST * VarList;

public:
	ExternFunctionAST(string name, TypeAST * returntype, VarDefAST * varlist): Name(name), ReturnType(returntype), VarList(varlist) {}
	~ExternFunctionAST() {
		if( VarList ) delete VarList;
		if( ReturnType ) delete ReturnType;
	}

	string str() {
		return string("ExternFunction")  + "(" + Name + "," + ReturnType->str() + "," + getString(VarList) + ")";
	}
};

// field_size = Scalar | Array(int array_size)
enum VAR_TYPE { VAR_TYPE_SCALAR, VAR_TYPE_ARRAY };
class VarSizeAST : public decafAST {
private:
	VAR_TYPE VarType;
	string Size;
public:
	VarSizeAST(VAR_TYPE type, string size): VarType(type), Size(size) {};

	string str() {
		if( VarType == VAR_TYPE_SCALAR)
			return string("Scalar");
		else
			return string("Array(") + Size + ")";
	} 
};

// FieldDecl(identifier name, decaf_type type, field_size size)
class FieldDeclAST : public decafAST {
	string Name;
	TypeAST * Type;
	VarSizeAST * field_size;

public:
	FieldDeclAST(string name, TypeAST * type, VarSizeAST * size): Name(name), Type(type), field_size(size) { };
	~FieldDeclAST() { if (Type) delete Type; if(field_size) delete field_size;};

	string str(){
		return string("FieldDecl(") + Name + "," + getString(Type) + "," + getString(field_size) + ")";  
	}

};

class StringConstantAST: public decafAST {
	string Value;
	public:
	StringConstantAST(string v): Value(v) {};
	~StringConstantAST() {};

	string str() {
		return string("StringConstant(") + Value + ")";
	}
};

class ValueExpr : public decafAST {};

// NumberExpr(int value)
class NumberExpr : public ValueExpr {
	string Value;

	public:
		NumberExpr(string value): Value(value) { };
		~NumberExpr() {};
		string str() {
			return string("NumberExpr(") + Value + ")";
		}
};

// BoolExpr(bool value)
class BoolExpr : public ValueExpr {
	bool Value;
	
public:
	BoolExpr(bool value): Value(value) {};
	~BoolExpr() {};

	string str() {
		return string("BoolExpr(") + ( Value ? "True":"False" ) + ")";
	}
};


// AssignGlobalVar(identifier name, decaf_type type, constant value)
class AssignGlobalVar : public decafAST {
	string Name;
	TypeAST * Type;
	ValueExpr * Value;

	public:
		AssignGlobalVar(string name, TypeAST * type,ValueExpr * value): Name(name), Type(type), Value(value) {} ;
		~AssignGlobalVar() {
			if ( Type ) delete Type;
			if ( Value ) delete Value;
		}
	
	string str() {
		return string("AssignGlobalVar(") + Name + "," + getString(Type) + "," + getString(Value) + ")";
	}
};

// MethodBlock(typed_symbol* var_decl_list, statement* statement_list)
class MethodBlockAST: public decafAST {
	decafStmtList * VarDeclList;
	decafStmtList * StatementList;

	public:
		MethodBlockAST(decafStmtList* var_decl_list, decafStmtList * statement_list): VarDeclList(var_decl_list), StatementList(statement_list) {}
		~MethodBlockAST(){
			if( VarDeclList ) delete VarDeclList;
			if( StatementList ) delete StatementList;
		}

		string str() {
			return string("MethodBlock(") + getString(VarDeclList)+"," + getString(StatementList) + ")";
		}
};

// Block(typed_symbol* var_decl_list, statement* statement_list)
class BlockAST: public decafAST {
	decafStmtList * VarDeclList;
	decafStmtList * StatementList;

	public:
		BlockAST(decafStmtList* var_decl_list, decafStmtList * statement_list): VarDeclList(var_decl_list), StatementList(statement_list) {}
		~BlockAST(){
			if( VarDeclList ) delete VarDeclList;
			if( StatementList ) delete StatementList;
		}

		string str() {
			return string("Block(") + getString(VarDeclList)+"," + getString(StatementList) + ")";
		}
};

// Method(identifier name, method_type return_type, typed_symbol* param_list, method_block block)
class MethodAST : public decafAST {
	string Name;
	TypeAST *  ReturnType;
	decafStmtList * ParamList; // VarDefAST*
	MethodBlockAST * Block;

	public:
		MethodAST(string name, TypeAST * return_type, decafStmtList * param_list, MethodBlockAST * block): 
			Name(name), ReturnType(return_type), ParamList(param_list), Block(block) {};

		~MethodAST(){
			if( ReturnType ) delete ReturnType;
			if( ParamList ) delete ParamList;
			if( Block ) delete Block;
		}

		string str() {
			return string("Method(") + Name + "," + getString(ReturnType) + "," + getString(ParamList) + "," + getString(Block) + ")";
		}
};

class ExprAST: public decafAST {};

// AssignArrayLoc(identifier name, expr index, expr value)
class AssignArrayLocAST : public decafAST {
	string Name;
	ExprAST * IndexExpr;
	ExprAST * ValueExpr;

	public:
	AssignArrayLocAST(string name, ExprAST * index, ExprAST * value): Name(name), IndexExpr(index), ValueExpr(value) {};
	~AssignArrayLocAST() {
		if ( IndexExpr ) delete IndexExpr;
		if ( ValueExpr ) delete ValueExpr;
	}

	string str() {
		return string("AssignArrayLoc(") + Name + "," + getString(IndexExpr) + "," + getString(ValueExpr) + ")";
	}
};

// AssignVar(identifier name, expr value)
class AssignVarAST: public decafAST {
	string Name;
	ExprAST * Expr;

public:
	AssignVarAST(string name, ExprAST * expr): Name(name), Expr(expr) {};
	~AssignVarAST() {
		if( Expr ) delete Expr;
	};

	string str() {
		return string("AssignVar(") + Name + "," + getString(Expr) + ")";
	}
};

// rvalue = VariableExpr(identifier name)
//     | ArrayLocExpr(identifier name, expr index)
class VariableExprAST: public decafAST {
	string Id;
public:
	VariableExprAST(string id): Id(id) {};
	~VariableExprAST() {};

	string str() {
		return string("VariableExpr(") + Id + ")";
	}
};

class ArrayLocExprAST: public decafAST {
	string Id;
	ExprAST * IndexExpr;

public:
	ArrayLocExprAST(string id, ExprAST * expr): Id(id), IndexExpr(expr) {};
	~ArrayLocExprAST() {
		if( IndexExpr ) delete IndexExpr;
	}

	string str() {
		return string("ArrayLocExpr(") + Id + "," + getString(IndexExpr) + ")";
	}

};

// MethodCall(identifier name, method_arg* method_arg_list)
class MethodCallAST: public decafAST {
	string Name;
	decafStmtList * MethodArgList;
public:
	MethodCallAST(string name, decafStmtList * method_arg_list): Name(name), MethodArgList(method_arg_list) {};
	~MethodCallAST() {
		if ( MethodArgList ) delete MethodArgList;
	}

	string str() {
		return string("MethodCall(") + Name + "," + getString(MethodArgList) + ")";
	}
};

// BinaryExpr(binary_operator op, expr left_value, expr right_value)
class BinaryExprAST: public decafAST {
	string Op; // {Plus , Minus , Mult , Div , Leftshift , Rightshift , Mod , Lt , Gt , Leq , Geq , Eq , Neq , And , Or};
	ExprAST * LeftExpr;
	ExprAST * RightExpr;

	public:
	BinaryExprAST(string op, ExprAST * left, ExprAST * right): Op(op), LeftExpr(left), RightExpr(right) {};
	~BinaryExprAST() {
		if ( LeftExpr ) delete LeftExpr;
		if ( RightExpr ) delete RightExpr;
	}
	
	string str() {
		return string("BinaryExpr(") + Op + "," + getString(LeftExpr) + "," + getString(RightExpr) + ")";
	}

};

// UnaryExpr(unary_operator op, expr value)
class UnaryExprAST: public decafAST {
	string Op; // UnaryMinus | Not
	ExprAST * Expr;

	public:
		UnaryExprAST(string op, ExprAST * expr): Op(op), Expr(expr) {};
		~UnaryExprAST() {
			if ( Expr ) delete Expr;
		}

		string str() {
			return string("UnaryExpr(") + Op + "," + getString(Expr) + ")";
		}
};


// IfStmt(expr condition, block if_block, block? else_block)
class IfStmtAST: public decafAST {
	ExprAST * ConditionExpr;
	BlockAST * IfBlock;
	BlockAST * ElseBlock;

	public:
		IfStmtAST(ExprAST * condition, BlockAST * ifblock, BlockAST * elseblock): ConditionExpr(condition), IfBlock(ifblock), ElseBlock(elseblock) {};
		~IfStmtAST() {
			if ( ConditionExpr ) delete ConditionExpr;
			if ( IfBlock ) delete IfBlock;
			if ( ElseBlock ) delete ElseBlock;
		}

		string str() {
			return string("IfStmt(") + getString(ConditionExpr) + "," + getString(IfBlock) + "," + getString(ElseBlock) + ")";
		}
};

// WhileStmt(expr condition, block while_block)
class WhileStmtAST: public decafAST {
	ExprAST * ConditionExpr;
	BlockAST * WhileBlock;

	public:
	WhileStmtAST(ExprAST * cond, BlockAST * block): ConditionExpr(cond), WhileBlock(block) {} ;
	~WhileStmtAST() {
		if ( ConditionExpr ) delete ConditionExpr;
		if ( WhileBlock ) delete WhileBlock;
	}

	string str() {
		return string("WhileStmt(") + getString(ConditionExpr) + "," + getString(WhileBlock) + ")";
	}
};

// ForStmt(assign* pre_assign_list, expr condition, assign* loop_assign_list, block for_block)
class ForStmtAST: public decafAST {
	decafStmtList * PreAssignList;
	ExprAST * Condition;
	decafStmtList * LoopAssignList;
	BlockAST * ForBlock;
	public:
		ForStmtAST(decafStmtList * pre_assign_list, ExprAST * condition, decafStmtList * loop_assign_list, BlockAST * for_block):
			PreAssignList(pre_assign_list), Condition(condition), LoopAssignList(loop_assign_list), ForBlock(for_block) {};
		~ForStmtAST() {
			if ( PreAssignList ) delete PreAssignList;
			if ( Condition ) delete Condition;
			if ( LoopAssignList ) delete LoopAssignList;
			if ( ForBlock ) delete ForBlock;
		}

		string str() {
			return string("ForStmt(") + getString(PreAssignList) + "," + getString(Condition) + "," + getString(LoopAssignList) + "," + getString(ForBlock) + ")";
		}
};

// ReturnStmt(expr? return_value)
class ReturnStmtAST: public decafAST {
	ExprAST * ReturnValue;
	public:
		ReturnStmtAST(ExprAST * return_value): ReturnValue(return_value) {};
		~ReturnStmtAST() {
			if ( ReturnValue ) delete ReturnValue;
		}
		string str() {
			return string("ReturnStmt(") + getString(ReturnValue) + ")"; 
		}
};

// BreakStmt
class BreakStmtAST: public decafAST {
	public:
	string str() { return string("BreakStmt"); }
};

// ContinueStmt
class ContinueStmtAST: public decafAST {
	public:
	string str() { return string("ContinueStmt"); }

};