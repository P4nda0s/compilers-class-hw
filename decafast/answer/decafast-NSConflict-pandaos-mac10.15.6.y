%{
#include <iostream>
#include <ostream>
#include <string>
#include <cstdlib>
#include <vector>
#include "default-defs.h"

int yylex(void);
int yyerror(char *); 

// print AST
bool printAST = true;
#include "decafast.cc"
using namespace std;

%}

%union{
    class decafAST *ast;
    std::string *sval;
    std::vector<std::string> *svals;
 }

%token T_PACKAGE
%token T_LCB
%token T_RCB
%token <sval> T_ID T_STRINGTYPE T_INTTYPE T_BOOLTYPE  T_VOID T_INTCONSTANT T_CHARCONSTANT
%token T_N_TOKEN T_FUNC T_INT T_LPAREN T_RPAREN T_WHITESPACE T_WHITESPACE_N T_AND T_ASSIGN  T_BREAK T_COMMA T_COMMENT  T_CONTINUE  T_DIV  T_DOT  T_ELSE  T_EQ  T_EXTERN  T_FALSE  T_FOR  T_GEQ  T_GT  T_IF T_LEFTSHIFT  T_LEQ  T_LSB  T_LT  T_MINUS  T_MOD  T_MULT T_NEQ  T_NOT  T_NULL  T_OR  T_PLUS  T_RIGHTSHIFT  T_RSB  T_SEMICOLON  T_STRINGCONSTANT T_TRUE  T_VAR  T_WHILE  T_RETURN
%type <ast> return_type extern_list decafpackage extern_stmt extern_var_list ExternType program field_list method_list FieldDecl normal_type constant_expr T_TRUE T_FALSE
%type <svals> comma_id_list 
%nonassoc LOWER_THAN_T_ID
%nonassoc T_ID
%nonassoc HIGHER_THAN_T_ID
%%

start: program

program: extern_list decafpackage
    { 
        ProgramAST *prog = new ProgramAST((decafStmtList *)$1, (PackageAST *)$2); 
		if (printAST) {
			cout << getString(prog) << endl;
		}
        delete prog;
    }

extern_list: /* extern_list can be empty */
    { decafStmtList *slist = new decafStmtList(); $$ = slist; }
    | extern_list extern_stmt { decafStmtList *slist = (decafStmtList *) $1; slist->push_back($2); $$ = slist; }

extern_stmt: T_EXTERN T_FUNC T_ID T_LPAREN extern_var_list T_RPAREN return_type T_SEMICOLON
    {
        VarDefAST * ast_var_def = new VarDefAST((decafStmtList *)$5);
        ExternFunctionAST * ast_extern_function = new ExternFunctionAST(*$3, (TypeAST * )$7, ast_var_def);
        delete $3;
        $$ = ast_extern_function;
    }


extern_var_list: 
    { decafStmtList *slist = new decafStmtList(); $$ = slist;}
    | extern_var_list ExternType T_COMMA  { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }
    | extern_var_list ExternType   { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist;  }

ExternType: T_STRINGTYPE { $$ = new TypeAST($1); } 
        | T_INTTYPE { $$ = new TypeAST($1); }
        | T_BOOLTYPE { $$ = new TypeAST($1); }

return_type: T_VOID { $$ = new TypeAST($1);}
    | T_BOOLTYPE { $$ = new TypeAST($1); }
    | T_INTTYPE { $$ = new TypeAST($1); }
    | T_STRINGTYPE { $$ = new TypeAST($1); }

normal_type: 
    T_BOOLTYPE { $$ = new TypeAST($1); }
    | T_INTTYPE { $$ = new TypeAST($1); }
    | T_STRINGTYPE { $$ = new TypeAST($1); }

decafpackage: T_PACKAGE T_ID T_LCB field_list method_list T_RCB
    { $$ = new PackageAST(*$2, new decafStmtList(), new decafStmtList()); delete $2; }
    ;
    

field_list:
    { decafStmtList * slist = new decafStmtList(); $$ = slist; }
    | field_list FieldDecl {
        decafStmtList * slist = (decafStmtList *)$1;
        decafStmtList * slist2 = (decafStmtList *)$2;
        slist2->move_to(slist);
        delete slist2;
        std::cout << getString(slist) << endl;
        $$ = slist;
    }
constant_expr: T_INTCONSTANT {NumberExpr * ne = new NumberExpr(*$1); delete $1; $$ = ne; }
    | T_TRUE { BoolExpr * be = new BoolExpr(true); $$ = be;}
    | T_FALSE { BoolExpr * be = new BoolExpr(false); $$ = be;}
    | T_CHARCONSTANT {NumberExpr * ne = new NumberExpr(*$1); delete $1; $$ = ne; }

FieldDecl:
    T_VAR comma_id_list normal_type T_SEMICOLON 
    {
        /*
        FieldDecls = { FieldDecl } .
        FieldDecl  = var { identifier }+, Type ";" .
        FieldDecl  = var { identifier }+, ArrayType ";" .
        FieldDecl  = var identifier Type "=" Constant ";" .
        */
        decafStmtList * slist = new decafStmtList();
        for(std::vector<std::string>::iterator i = $2->begin(); i != $2->end() ; i++){
            std::string var_name = *i;
            VarSizeAST * sizedecl = new VarSizeAST(VAR_TYPE_SCALAR, "1");
            FieldDeclAST * decl = new FieldDeclAST(var_name, (TypeAST *)$3, sizedecl); 
            slist->push_back( (decafAST *)decl );
        }
        delete $2;
        $$ = slist;
    }
    | T_VAR comma_id_list T_LSB T_INTCONSTANT T_RSB normal_type T_SEMICOLON 
    {
        
        decafStmtList * slist = new decafStmtList();
        for(std::vector<std::string>::iterator i = $2->begin(); i != $2->end() ; i++){
            std::string var_name = *i;
            VarSizeAST * sizedecl = new VarSizeAST(VAR_TYPE_ARRAY, *$4);
            FieldDeclAST * decl = new FieldDeclAST(var_name, (TypeAST *)$6, sizedecl); 
            slist->push_back( (decafAST *)decl );   
        }
        delete $2;
        delete $4;
        $$ = slist;
    }
    | T_VAR T_ID normal_type T_ASSIGN constant_expr  T_SEMICOLON
    {
        printf("fuck you..\n");
        // decafStmtList * slist = new decafStmtList();
        // AssignGlobalVar * assign = new AssignGlobalVar(*$2, (TypeAST *)$3, (ValueExpr *) $5);
        // delete $2;
        // slist->push_back(assign);
        // $$ = slist;
    }
    

comma_id_list: comma_id_list T_COMMA T_ID %prec LOWER_THAN_T_ID { std::vector<std::string> * ids = $1; ids->push_back(*$3); $$ = ids; delete $3; } 
    | T_ID  { std::vector<std::string>  * ids = new std::vector<std::string>(); ids->push_back(*$1); $$ = ids; delete $1;} 

method_list: T_ID T_ID T_ID T_SEMICOLON

%%

int main1() {
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

