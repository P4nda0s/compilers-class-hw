
#ifndef _DECAF_DEFS
#define _DECAF_DEFS

/// header files for llvm
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/GlobalVariable.h>

#include <cstdio> 
#include <cstdlib>
#include <cstring> 
#include <string>
#include <stdexcept>
#include <vector>

extern int yylineno;
extern int tokenpos;

using namespace std;

/// definiation for symbol table
enum SymbolType {SYM_VAR, SYM_FUNCTION, SYM_FIELD, SYM_BB};

struct descriptor {
	SymbolType type;
	int lineo;
	string name;
	llvm::Value * target;
};
typedef map<string, descriptor *> symbol_table;
typedef list<symbol_table *> symbol_table_list;


extern "C"
{
	extern int yyerror(const char *);
	int yyparse(void);
	int yylex(void);  
	int yywrap(void);
}

#endif

