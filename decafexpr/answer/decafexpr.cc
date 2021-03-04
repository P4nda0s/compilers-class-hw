
#include "default-defs.h"
#include <list>
#include <map>
#include <ostream>
#include <iostream>
#include <sstream>
#include <string>
#ifndef YYTOKENTYPE
#include "decafexpr.tab.h"
#endif


using namespace std;
symbol_table_list symtbl;

// this global variable contains all the generated code
static llvm::Module *TheModule;
static llvm::LLVMContext TheContext;
// this is the method used to construct the LLVM intermediate code (IR)
static llvm::IRBuilder<> Builder(TheContext);


bool replace(std::string& str, const std::string& from, const std::string& to) {
    size_t start_pos = str.find(from);
    if(start_pos == std::string::npos)
        return false;
    str.replace(start_pos, from.length(), to);
    return true;
}


llvm::Value * decafTyCast(llvm::Type * targetTy, llvm::Value * from) {
	// decaf type caster.....
	if (targetTy->isIntegerTy()) {
		if(from->getType()->getIntegerBitWidth() == 1) { // bool to int32
			return Builder.CreateZExt(from, targetTy, "zexttmp");
		}
	}
	return nullptr;
}

llvm::Constant * decafGetInitalizer(llvm::Type * ty) {
	switch (ty->getTypeID())
	{
	case llvm::Type::IntegerTyID:
		return llvm::ConstantInt::get(ty, llvm::APInt(ty->getIntegerBitWidth(), 0, true));
		break;
	default:
		break;
	}
}

// functions for symbol table
bool scope_enter() {
	symtbl.push_back(new symbol_table());
	return true;
}

bool scope_leave() {
	symbol_table * st = symtbl.back();
	for (auto [key, value] : *st) 
		delete value;

	symtbl.pop_back();
	delete st;
	return true;
}

bool symbol_enter(string name, SymbolType type, llvm::Value * target) {
	descriptor * desc = new descriptor;
	symbol_table * curSymbol = symtbl.back();
	if (curSymbol->count(name) != 0) {
		printf("Redefined variable %s at %d \n", name.c_str(), yylineno);	
		return false;
	}
	desc->name = name;
	desc->target = target;
	desc->type = type;

	(*curSymbol)[name] = desc;
	return true;
}

descriptor * symbol_get(string name) {
	for(symbol_table_list::reverse_iterator iter = symtbl.rbegin(); iter != symtbl.rend(); iter++) {
		symbol_table * st = *iter;
		if (st->count(name)) {
			return (*st)[name];
		}
	}
	printf("Undefined symbol %s at %d \n", name.c_str(), yylineno);
	return nullptr;
}

void displaySymbols() {
	string tab;
	std::cout << " ======= symbol table =========" << std::endl;
	for(auto scope: symtbl) {
		for (auto [key, value] : *scope) {
			std::cout << tab << key << std::endl;
		}
		tab += "\t";
	}
}

enum decafType { voidTy, intTy, boolTy, stringTy } ;
llvm::Type *getLLVMType(decafType ty) { 
    switch (ty) {
        case voidTy: return Builder.getVoidTy();
        case intTy: return Builder.getInt32Ty();
        case boolTy: return Builder.getInt1Ty();
        case stringTy: return Builder.getInt8PtrTy(); 
		default: 
			printf("unknow type\n");
			throw runtime_error("unknown type");
    } 
}


/// decafAST - Base class for all abstract syntax tree nodes.


enum decafBinaryOP {Plus , Minus , Mult , Div , Leftshift , Rightshift , Mod , Lt , Gt , Leq , Geq , Eq , Neq , And , Or};

class decafAST {
public:
  virtual ~decafAST() {}
  virtual string str() { return string(""); }
  virtual llvm::Value * codegen() { return nullptr; };
  virtual void ResolveSymbols() {};
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
			//printf("delete: %p \n", *i);
			//if(*i) delete *i;
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
	llvm::Value * codegen() {
		for( auto stmt : this->stmts) {
			stmt->codegen();
		}
		return nullptr;
	}
	
	list<decafAST *> & getlist() {
		return stmts;
	}
};

class TypeAST : public decafAST {
	string * Name;
	decafType decafTy;
public:
	TypeAST(string * name): Name(name) {
		if (*name == "StringType")
			decafTy = stringTy;
		else if(*name == "IntType")
			decafTy = intTy;
		else if(*name == "BoolType")
			decafTy = boolTy;
		else if(*name == "VoidType")
			decafTy = voidTy;
		else {
			printf("Unkown type:%s \n", name->c_str()) ;
		}
	};
	~TypeAST() { if(Name) delete Name; };
	string str() {
		return string(*Name);
	}
	decafType getType() {return decafTy;}
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

		decafType getType() {
			assert(TypeList->size() == 1 && Name != "");
			list<decafAST *> theType = TypeList->getlist();
			TypeAST * ta = (TypeAST *) theType.back();
			return ta->getType();
		}

		string getName() {
			return Name;
		}

		list<decafAST *> & getlist() {
			return TypeList->getlist();
		}

		llvm::Value * codegen() {
			return Builder.CreateAlloca(getLLVMType(getType()), 0, getName());
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

	string getName() {
		return Name;
	}

	llvm::Value * codegen() {
		llvm::Type * returnTy;
		std::vector<llvm::Type *> args;
		for (auto t : VarList->getlist()) {
			TypeAST * ty = (TypeAST *)t;
			args.push_back(getLLVMType(ty->getType()));
		}

		returnTy = getLLVMType(ReturnType->getType());

		llvm::Function *func = llvm::Function::Create(
			llvm::FunctionType::get(returnTy, args, false),
			llvm::Function::ExternalLinkage,
			Name,
			TheModule);
		
		return func;
		
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

	bool isArray() {
		return VarType == VAR_TYPE_ARRAY;
	}

	int getArraySize() { return stoi(Size); }
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

	string getName() { return Name; }
	llvm::Value * codegen() {
		llvm::Type * ty = getLLVMType(Type->getType());
		llvm::Value * v;
		if (field_size->isArray()) {
			llvm::ArrayType * arrayType = llvm::ArrayType::get(ty, field_size->getArraySize());
			llvm::Constant * initializer = llvm::ConstantAggregateZero::get(arrayType);
			v = new llvm::GlobalVariable(*TheModule, arrayType, false, llvm::GlobalVariable::CommonLinkage, initializer, Name);
			// initize array elements with 0.

		}else {
			v = new llvm::GlobalVariable(*TheModule, ty, false, 
				llvm::GlobalVariable::CommonLinkage, decafGetInitalizer(ty), Name);
		}
		
		symbol_enter(Name, SYM_FIELD, v);
		return nullptr;
	}
};
class ValueExpr : public decafAST {
public:
	virtual llvm::Constant * codegen()=0;
};

class StringConstantAST: public ValueExpr {
	string Value;
	public:
	StringConstantAST(string v): Value(v) {};
	~StringConstantAST() {};

	string str() {
		return string("StringConstant(") + Value + ")";
	}

	llvm::Constant * codegen() {
		string content = Value.substr(1, Value.size() - 2);
		replace(content, "\\n", "\n");
		replace(content, "\\t", "\t");
		return Builder.CreateGlobalStringPtr(content, "gs", 0, TheModule);
	}
};



// NumberExpr(int value)
class NumberExpr : public ValueExpr {
	string Value;

	public:
		NumberExpr(string value): Value(value) { };
		~NumberExpr() {};
		string str() {
			return string("NumberExpr(") + Value + ")";
		}

		llvm::Constant * codegen() {
			if ((Value.find("0x") != std::string::npos) || (Value.find("0X") != std::string::npos)) 
				return Builder.getInt32(stoi(Value, nullptr, 16));
			return Builder.getInt32(stoi(Value));

			
			
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

	llvm::Constant * codegen() {
		return Builder.getInt1((int)Value);
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

	string getName() { return Name; }
	llvm::Value * codegen() {
		llvm::Type * ty = getLLVMType(Type->getType());
		llvm::Constant * initalValue = Value->codegen();

		llvm::Value * v = new llvm::GlobalVariable(*TheModule, ty, false, 
			llvm::GlobalVariable::CommonLinkage, initalValue, Name);
		symbol_enter(Name, SYM_FIELD, v);
		return nullptr;
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

		llvm::Value * codegen() {
			// scope create from MethodAST.
			// allocate stack space for varibables.
			for (auto typedsymbol : VarDeclList->getlist()) {
				VarDefAST * vardef = (VarDefAST *) typedsymbol;
				symbol_enter(vardef->getName(), SYM_VAR, vardef->codegen());
			}
			// codegen for Statements
			// displaySymbols();
			StatementList->codegen();
			return nullptr;
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
		
		llvm::Value * codegen() {
			scope_enter();
			for (auto typedsymbol : VarDeclList->getlist()) {
				VarDefAST * vardef = (VarDefAST *) typedsymbol;
				symbol_enter(vardef->getName(), SYM_VAR, vardef->codegen());
			}
			StatementList->codegen();
			scope_leave();
			return nullptr;
		}
};

// Method(identifier name, method_type return_type, typed_symbol* param_list, method_block block)
class MethodAST : public decafAST {
	string Name;
	TypeAST *  ReturnType;
	decafStmtList * ParamList; // VarDefAST*
	MethodBlockAST * Block;
	llvm::Function * declare;
	public:
		MethodAST(string name, TypeAST * return_type, decafStmtList * param_list, MethodBlockAST * block): 
			Name(name), ReturnType(return_type), ParamList(param_list), Block(block), declare(nullptr) {};

		~MethodAST(){
			if( ReturnType ) delete ReturnType;
			if( ParamList ) delete ParamList;
			if( Block ) delete Block;
		}

		string str() {
			return string("Method(") + Name + "," + getString(ReturnType) + "," + getString(ParamList) + "," + getString(Block) + ")";
		}

		llvm::Value * codegen() {
			// codegen for method declare
			llvm::Type * returnTy;
			std::vector<llvm::Type *> args;
			list<decafAST *> argumentList;
			argumentList = ParamList->getlist();
			for (auto t : argumentList) {
				VarDefAST * ty = (VarDefAST *)t;
				args.push_back(getLLVMType(ty->getType()));
			}

			returnTy = getLLVMType(ReturnType->getType());
			declare = llvm::Function::Create(
				llvm::FunctionType::get(returnTy, args, false),
				llvm::Function::ExternalLinkage,
				Name,
				TheModule);
			// set names.
			unsigned int idx = 0;
			auto arg_iter = argumentList.begin();
			for (auto & arg : declare->args()) {
				VarDefAST * ty = (VarDefAST *) *arg_iter++;
				arg.setName(ty->getName());
			}

			symbol_enter(Name, SYM_FUNCTION, declare);
			return declare;
		}

		llvm::Value * codegen_body() {
			// codegen for method body..
			llvm::BasicBlock * BB = llvm::BasicBlock::Create(TheContext, "entry", declare);
			Builder.SetInsertPoint(BB);

			// insert arguments' name into symbol table..
			scope_enter();
			for (auto & Arg : declare->args()) {
				llvm::Value * va_arg = &Arg;
				llvm::Value * alloc = Builder.CreateAlloca(va_arg->getType(), 0, va_arg->getName());
				Builder.CreateStore(va_arg, alloc);
				symbol_enter(Arg.getName().str(), SYM_VAR, alloc);
			}
			Block->codegen();
			// check has return statm.
			for (auto & bb : declare->getBasicBlockList()) {
				llvm::Instruction *Terminator = bb.getTerminator();
				if (!Terminator) {
					llvm::Type * ty = declare->getReturnType();
					Builder.SetInsertPoint(&bb);
					if (ty->isVoidTy()){
						Builder.CreateRetVoid();
					}else if(ty->isIntegerTy()) {
						if(ty->getIntegerBitWidth() == 1)
							Builder.CreateRet(Builder.getInt1(0));
						else
							Builder.CreateRet(Builder.getInt32(0));
					}
				}
			}
			scope_leave();
			llvm::verifyFunction(*declare);
			return nullptr;
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

	llvm::Value * codegen() {
		descriptor * sym = symbol_get(Name);
		if (!sym) exit(EXIT_FAILURE);
		llvm::Value * arr = sym->target;
		llvm::Value * idx = IndexExpr->codegen();
		if (!idx->getType()->isIntegerTy()) {
			printf("Expected a integer index. \n");
			exit(EXIT_FAILURE);
		}
		if ( !arr->getType()->getPointerElementType()->isArrayTy() ) {
			//arr->getType()->dump();
			printf("%s is not array.\n", Name.c_str());
			exit(EXIT_FAILURE);
		}
		llvm::Value * lvalue = Builder.CreateGEP(arr, {Builder.getInt32(0), idx}, "arr");
		llvm::Value * rvalue = ValueExpr->codegen();
		if ( lvalue->getType()->getPointerElementType() !=  rvalue->getType() ) {
			rvalue = decafTyCast(lvalue->getType(), rvalue);
			if(!rvalue) {
				printf("Typecheck array elements type != ValueExpr type \n");
				exit(EXIT_FAILURE);
			}
		}
		Builder.CreateStore(rvalue, lvalue);
		return nullptr;
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
	llvm::Value * codegen() {
		descriptor * sym = symbol_get(Name);
		if ( !sym )
			exit(EXIT_FAILURE);

		llvm::Value * rvalue = Expr->codegen();
		llvm::Value * lvalue = sym->target; 
		// semantic check
		
		const llvm::PointerType *ptrTy = rvalue->getType()->getPointerTo();
		if (lvalue->getType() != ptrTy) {
			rvalue = decafTyCast(lvalue->getType()->getPointerElementType(), rvalue);
			if (!rvalue) {
				printf("Typecheck error for %s \n", Name.c_str());
				exit(EXIT_FAILURE);
			}
		}
		Builder.CreateStore(rvalue, lvalue);
		return nullptr;
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

	llvm::Value * codegen() {
		descriptor * sym = symbol_get(Id);
		if(!sym) exit(EXIT_FAILURE);
		//sym->target->dump();
		return Builder.CreateLoad(sym->target, Id);
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
	llvm::Value * codegen() {
		descriptor * sym = symbol_get(Id);
		if (!sym) exit(EXIT_FAILURE);

		llvm::Value * index = IndexExpr->codegen();

		if (!index->getType()->isIntegerTy()) {
			printf("Expected integer index. \n");
			exit(EXIT_FAILURE);
		}

		if ( !sym->target->getType()->getPointerElementType()->isArrayTy() ) {
			printf("Expected array. \n");
			exit(EXIT_FAILURE);
		}

		llvm::Value * ptr = Builder.CreateGEP(sym->target, {Builder.getInt32(0), index});
		return Builder.CreateLoad(ptr);
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

	llvm::Value * codegen() {
		llvm::Function *call;
		std::vector<llvm::Value *> args;
		descriptor * sym = symbol_get(Name);
		auto passArgs = MethodArgList->getlist();
		if ( !sym ) exit(EXIT_FAILURE);
		call = ( llvm::Function * )sym->target;

		// semantic checks
		if (call->arg_size() != passArgs.size()) {
			printf("Arguments size error for: %s \n", Name.c_str());
			exit(EXIT_FAILURE);
		}

		auto iter_passin = passArgs.begin();
		for (auto & arg : call->args()) {
			llvm::Value * pvalue  = (*iter_passin++)->codegen();
			if( pvalue->getType() != arg.getType()){
				pvalue = decafTyCast(arg.getType(), pvalue);
				if (!pvalue) {
					printf("Typecheck error : %s \n", Name.c_str());
					exit(EXIT_FAILURE);
				}
			}
			args.push_back(pvalue);
		}
		bool isVoid = call->getReturnType()->isVoidTy();
		llvm::Value *val = Builder.CreateCall(
			call,
			args,
			isVoid ? "" : "calltmp"
		);
		return val;
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
	
	llvm::Value * codegen() {
		llvm::Value * value1 = LeftExpr->codegen();
		llvm::Value * value2;
		if(Op == "And") {
			llvm::BasicBlock * bbori = Builder.GetInsertBlock();
			llvm::Function   * func  = bbori->getParent();
			llvm::BasicBlock * bb1   = llvm::BasicBlock::Create(TheContext, "And1", func);
			llvm::BasicBlock * bb2   = llvm::BasicBlock::Create(TheContext, "AndEnd", func);
			llvm::BasicBlock * bbcur = nullptr;
			Builder.CreateCondBr(value1, bb1, bb2);
			Builder.SetInsertPoint(bb1);
			value2 = RightExpr->codegen();
			bbcur  = Builder.GetInsertBlock(); // The RHS may create new BBs. 
			Builder.CreateBr(bb2);
			Builder.SetInsertPoint(bb2);

			llvm::PHINode * node = Builder.CreatePHI(getLLVMType(boolTy), 2, "phiAnd");
			node->addIncoming(value1, bbori); // from original bb.
			node->addIncoming(value2, bbcur);   // from and 2 bb.

			return node;
		}else if(Op == "Or") {
			llvm::BasicBlock * bbori = Builder.GetInsertBlock();
			llvm::Function   * func  = bbori->getParent();
			llvm::BasicBlock * bb1   = llvm::BasicBlock::Create(TheContext, "Or1", func);
			llvm::BasicBlock * bb2   = llvm::BasicBlock::Create(TheContext, "OrEnd", func);
			llvm::BasicBlock * bbcur = nullptr;

			Builder.CreateCondBr(value1, bb2, bb1);
			Builder.SetInsertPoint(bb1);
			value2 = RightExpr->codegen();
			bbcur  = Builder.GetInsertBlock(); // The RHS may create new BBs. 
			Builder.CreateBr(bb2);
			Builder.SetInsertPoint(bb2);

			llvm::PHINode * node = Builder.CreatePHI(getLLVMType(boolTy), 2, "phiOR");
			node->addIncoming(value1, bbori); // from original bb.
			node->addIncoming(value2, bbcur); // from and 2 bb.
			
			return node;
		}

		value2 = RightExpr->codegen();
		// semantic check
		if ( value1->getType() != value2->getType() ) {
			printf("Typecheck error in BinaryExprAST \n");
			exit(EXIT_FAILURE);
		}

		if (Op == "Plus") {
			return Builder.CreateAdd(value1, value2, "plustmp");
		}else if (Op == "Minus") {
			return Builder.CreateSub(value1, value2, "minustmp");
		}else if (Op == "Mult") {
			return Builder.CreateMul(value1, value2, "multtmp");
		}else if (Op == "Div") {
			return Builder.CreateSDiv(value1, value2, "divtmp");
		}else if (Op == "Leftshift") {
			return Builder.CreateShl(value1, value2, "leftshifttmp");
		}else if (Op == "Rightshift") {
			return Builder.CreateLShr(value1, value2, "rightshittmp");
		}else if(Op == "Mod") {
			return Builder.CreateSRem(value1, value2, "modtmp");
		}else if(Op == "Lt") {
			return Builder.CreateICmpSLT(value1, value2, "lttmp");
		}else if(Op == "Gt") {
			return Builder.CreateICmpSGT(value1, value2, "gttmp");
		}else if(Op == "Leq") {
			return Builder.CreateICmpSLE(value1, value2, "leqtmp");
		}else if(Op == "Geq") {
			return Builder.CreateICmpSGE(value1, value2, "geqtmp");
		}else if(Op == "Eq") {
			return Builder.CreateICmpEQ(value1, value2, "eqtmp");
		}else if(Op == "Neq") {
			return Builder.CreateICmpNE(value1, value2, "neqtmp");
		}
		return nullptr;
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
	llvm::Value * codegen() {
		llvm::Value * val = Expr->codegen();
		if ( Op == "Not" ) {
			return Builder.CreateNot(val);
		}else {
			return Builder.CreateNeg(val);
		}
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

		llvm::Value * codegen() {
			llvm::Value      * cond = ConditionExpr->codegen();
			llvm::BasicBlock * BBoriginal = Builder.GetInsertBlock();
			llvm::Function   * func    = BBoriginal->getParent();
			llvm::BasicBlock * BBTrue  = llvm::BasicBlock::Create(TheContext, "IfTrue", func);
			llvm::BasicBlock * BBFalse = llvm::BasicBlock::Create(TheContext, "IfFalse", func);
			llvm::BasicBlock * BBCont  = llvm::BasicBlock::Create(TheContext, "IfCont", func);

			if (cond->getType() != getLLVMType(boolTy)) {
				if (cond->getType()->isIntegerTy()) {
					cond = Builder.CreateICmpNE(cond, Builder.getInt32(0));
				}else {
					printf("Expected a Int or Bool expression.. \n");
					exit(EXIT_FAILURE);
				}
			}

			Builder.CreateCondBr(cond, BBTrue, BBFalse);
			Builder.SetInsertPoint(BBTrue);
			IfBlock->codegen();
			if(!Builder.GetInsertBlock()->getTerminator())
				Builder.CreateBr(BBCont);

			Builder.SetInsertPoint(BBFalse);
			if (ElseBlock) ElseBlock->codegen();

			if(!Builder.GetInsertBlock()->getTerminator())
				Builder.CreateBr(BBCont);

			Builder.SetInsertPoint(BBCont);
			return nullptr;
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

	llvm::Value * codegen() {
		llvm::BasicBlock * BBori = Builder.GetInsertBlock();
		llvm::Function   * func = BBori->getParent();
		llvm::BasicBlock * BBwhileStart = llvm::BasicBlock::Create(TheContext, "whileStart", func); // Create a BB for 'continue'
		llvm::BasicBlock * BBwhileBody  = llvm::BasicBlock::Create(TheContext, "whileBody" , func);
		llvm::BasicBlock * BBwhileEnd   = llvm::BasicBlock::Create(TheContext, "whileEnd"  , func);
		scope_enter();
		symbol_enter("LoopStart", SYM_BB, BBwhileStart);  //  continue;
		symbol_enter("LoopEnd",   SYM_BB, BBwhileEnd); // break;
		Builder.CreateBr(BBwhileStart); 

		Builder.SetInsertPoint(BBwhileStart); //BBori -> BBwhileStart
		llvm::Value * cond = ConditionExpr->codegen();
		if (cond->getType() != getLLVMType(boolTy)) {
			if (cond->getType()->isIntegerTy()) {
				cond = Builder.CreateICmpNE(cond, Builder.getInt32(0));
			}else {
				printf("Expected a Int or Bool expression.. \n");
				exit(EXIT_FAILURE);
			}
		}
		Builder.CreateCondBr(cond, BBwhileBody, BBwhileEnd);

		Builder.SetInsertPoint(BBwhileBody);
		WhileBlock->codegen();
		Builder.CreateBr(BBwhileStart);
		Builder.SetInsertPoint(BBwhileEnd); 
		scope_leave();
		return nullptr;
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

		llvm::Value * codegen() {
			llvm::Function * func = Builder.GetInsertBlock()->getParent();
			scope_enter();
			
			llvm::BasicBlock * BBForCond = llvm::BasicBlock::Create(TheContext, "BBForCond", func);
			llvm::BasicBlock * BBForLA   = llvm::BasicBlock::Create(TheContext, "BBForLA", func);
			llvm::BasicBlock * BBForBody = llvm::BasicBlock::Create(TheContext, "BBForBody", func);
			llvm::BasicBlock * BBForEnd  = llvm::BasicBlock::Create(TheContext, "BBForEnd", func);

			symbol_enter("LoopStart", SYM_BB, BBForLA); // continue
			symbol_enter("LoopEnd", SYM_BB, BBForEnd); // break
			PreAssignList->codegen();
			Builder.CreateBr(BBForCond);
			Builder.SetInsertPoint(BBForCond);
			llvm::Value * cond = Condition->codegen();
			if (cond->getType() != getLLVMType(boolTy)) {
				if (cond->getType()->isIntegerTy()) {
					cond = Builder.CreateICmpNE(cond, Builder.getInt32(0));
				}else {
					printf("Expected a Int or Bool expression.. \n");
					exit(EXIT_FAILURE);
				}
			}
			Builder.CreateCondBr(cond, BBForBody, BBForEnd);

			Builder.SetInsertPoint(BBForBody);
			ForBlock->codegen();

			if(!Builder.GetInsertBlock()->getTerminator())
				Builder.CreateBr(BBForLA);

			Builder.SetInsertPoint(BBForLA);
			LoopAssignList->codegen();
			Builder.CreateBr(BBForCond);

			Builder.SetInsertPoint(BBForEnd);
			scope_leave();
			return nullptr;
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

		llvm::Value * codegen() {
			llvm::Function * func = Builder.GetInsertBlock()->getParent();
			if (ReturnValue == nullptr){
				if (func->getReturnType()->isVoidTy())
					return Builder.CreateRetVoid();
				else{
					printf("ReturnStmt Error..Expected return noting..\n");
					exit(EXIT_FAILURE);
				}
			}
			llvm::Value * ret = ReturnValue->codegen();
			if (ret->getType() != func->getReturnType()) {
				ret = decafTyCast(func->getReturnType(), ret);
				if (!ret){
					printf("Funciotn return typecheck error \n");
					exit(EXIT_FAILURE);
				}
			}
			return Builder.CreateRet(ret);
		}
};

// BreakStmt
class BreakStmtAST: public decafAST {
	public:
	string str() { return string("BreakStmt"); }
	llvm::Value * codegen() {
		descriptor * sym = symbol_get("LoopEnd");
		if ( !sym ) exit(EXIT_FAILURE);
		Builder.CreateBr((llvm::BasicBlock *)sym->target);
		return nullptr;
	}
};

// ContinueStmt
class ContinueStmtAST: public decafAST {
	public:
	string str() { return string("ContinueStmt"); }
	llvm::Value * codegen() {
		descriptor * sym = symbol_get("LoopStart");
		if (!sym) exit(EXIT_FAILURE);
		Builder.CreateBr((llvm::BasicBlock *)sym->target);
		return nullptr;
	}
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

	llvm::Value * codegen() {
		FieldDeclList->codegen();
		MethodDeclList->codegen();

		for (auto method : MethodDeclList->getlist()) {
			MethodAST * m = (MethodAST *) method;
			m->codegen_body();
		}

		return nullptr;
	}
};

/// ProgramAST - the decaf program
// prog = Program(extern* extern_list, package body)
class ProgramAST : public decafAST {
	decafStmtList *ExternList;
	PackageAST *PackageDef;
public:
	ProgramAST(decafStmtList *externs, PackageAST *c) : ExternList(externs), PackageDef(c) {
		TheModule = new llvm::Module("Program1", TheContext);
	}
	~ProgramAST() { 
		if (ExternList != NULL) { delete ExternList; } 
		if (PackageDef != NULL) { delete PackageDef; }
	}
	string str() {return string("Program") + "(" + getString(ExternList) + "," + getString(PackageDef) + ")"; }


	llvm::Value * codegen() {
		
		scope_enter();
		for (auto external : ExternList->getlist()) {
			llvm::Value * v = nullptr;
			ExternFunctionAST * efun = (ExternFunctionAST *) external;
			
			v = efun->codegen();
			symbol_enter(efun->getName(), SYM_FUNCTION, v);
		}
		
		PackageDef->codegen();
		TheModule->print(llvm::errs(), nullptr);
		return nullptr;
	}
};
