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

string proceesCharLit(string charlit) {
    // a|b|t|n|v|f|r
    if (charlit[1] == '\\'){
        switch (charlit[2]){
            case 'n':
                return to_string('\n');
            case 'a':
                return to_string('\a');
            case 'b':
                return to_string('\b');
            case 't':
                return to_string('\t');
            case 'v':
                return to_string('\v');
            case 'f':
                return to_string('\f');
            case 'r':
                return to_string('\r');
            case '\\':
                return to_string('\\');
            case '\'':
                return to_string('\'');
            case '\"':
                return to_string('\"');
        }
    }
    return to_string(charlit[1]);
}
%}

%union{
    class decafAST *ast;
    std::string *sval;
    std::vector<std::string> *svals;
 }

%token T_PACKAGE
%token T_LCB
%token T_RCB
%token <sval> T_ID T_STRINGTYPE T_INTTYPE T_BOOLTYPE  T_VOID T_INTCONSTANT T_STRINGCONSTANT T_CHARCONSTANT
%token T_N_TOKEN T_FUNC T_INT T_LPAREN T_RPAREN T_WHITESPACE T_WHITESPACE_N T_AND T_ASSIGN  T_BREAK T_COMMA T_COMMENT  T_CONTINUE  T_DIV  T_DOT  T_ELSE  T_EQ  T_EXTERN  T_FALSE  T_FOR  T_GEQ  T_GT  T_IF T_LEFTSHIFT  T_LEQ  T_LSB  T_LT  T_MINUS  T_MOD  T_MULT T_NEQ  T_NOT  T_NULL  T_OR  T_PLUS  T_RIGHTSHIFT  T_RSB  T_SEMICOLON T_TRUE  T_VAR  T_WHILE  T_RETURN
%type <ast> return_type extern_list decafpackage extern_stmt extern_var_list ExternType program field_list method_list MethodDecl FieldDecl normal_type constant_expr T_TRUE T_FALSE typed_var_defs Block Statement Statements MethodArg MethodArgList ForAssignList  ElseBlock Expr Assign MethodCall VarDecl MethodBlock VarDecls
%type <svals> comma_id_list 

%nonassoc T_ID
%nonassoc HIGHIER_T_ID

%left T_OR
%left T_AND
%left T_EQ T_NEQ T_LT T_LEQ T_GT T_GEQ
%left T_PLUS T_MINUS
%left T_MULT T_DIV T_MOD T_RIGHTSHIFT T_LEFTSHIFT
%left UMINUS
%left UNOT
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
    | extern_var_list T_COMMA ExternType { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($3); $$ = slist; }
    | ExternType   { decafStmtList * slist = new decafStmtList(); slist->push_back($1); $$ = slist;  }

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
    { $$ = new PackageAST(*$2, (decafStmtList *)$4, (decafStmtList *)$5); delete $2; }


field_list:
    { decafStmtList * slist = new decafStmtList(); $$ = slist; }
    | field_list FieldDecl {
        decafStmtList * slist = (decafStmtList *)$1;
        decafStmtList * slist2 = (decafStmtList *)$2;
        slist2->move_to(slist);
        delete slist2;
      //  std::cout << getString(slist) << endl;
        $$ = slist;
    }

constant_expr: T_INTCONSTANT {NumberExpr * ne = new NumberExpr(*$1); delete $1; $$ = ne; }
    | T_TRUE { BoolExpr * be = new BoolExpr(true); $$ = be;}
    | T_FALSE { BoolExpr * be = new BoolExpr(false); $$ = be;}
    | T_CHARCONSTANT {NumberExpr * ne = new NumberExpr(proceesCharLit(*$1)); delete $1; $$ = ne; }
    | T_STRINGCONSTANT { StringConstantAST * sast = new StringConstantAST(*$1); $$ = sast; delete $1; }

FieldDecl:
    T_VAR  comma_id_list normal_type T_SEMICOLON 
    {
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
    | T_VAR  comma_id_list T_LSB T_INTCONSTANT T_RSB normal_type T_SEMICOLON 
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
    | T_VAR comma_id_list normal_type T_ASSIGN constant_expr T_SEMICOLON
    {
        std::vector<std::string> * ids = $2;
        if( ids->size() == 1){
            decafStmtList * slist = new decafStmtList();
            AssignGlobalVar * assign = new AssignGlobalVar(*ids->begin(), (TypeAST *)$3, (ValueExpr *) $5);
            delete $2;
            slist->push_back(assign);
            $$ = slist;
        }else{
            printf("error\n");
            //exit(0);
            YYABORT;
        }

    }

VarDecl:
    T_VAR  comma_id_list normal_type T_SEMICOLON 
    {
        decafStmtList * slist = new decafStmtList();
        for(std::vector<std::string>::iterator i = $2->begin(); i != $2->end() ; i++){
            std::string var_name = *i;
            VarDefAST * decl = new VarDefAST(var_name, (TypeAST *)$3); 
            slist->push_back( (decafAST *)decl );
        }
        delete $2;
        $$ = slist;
    }
    | T_VAR  comma_id_list T_LSB T_INTCONSTANT T_RSB normal_type T_SEMICOLON 
    {
        decafStmtList * slist = new decafStmtList();
        for(std::vector<std::string>::iterator i = $2->begin(); i != $2->end() ; i++){
            std::string var_name = *i;
            VarDefAST * decl = new VarDefAST(var_name, (TypeAST *)$6); 
            slist->push_back( (decafAST *)decl );   
        }
        delete $2;
        delete $4;
        $$ = slist;
    }

VarDecls:
    { decafStmtList * slist = new decafStmtList(); $$ = slist; }
    | VarDecls VarDecl {
        decafStmtList * slist = (decafStmtList *)$1;
        decafStmtList * slist2 = (decafStmtList *)$2;
        slist2->move_to(slist);
        delete slist2;
       // std::cout << getString(slist) << endl;
        $$ = slist;
    }

comma_id_list: comma_id_list T_COMMA T_ID  { std::vector<std::string> * ids = $1; ids->push_back(*$3); $$ = ids; delete $3; } 
    | T_ID  { std::vector<std::string>  * ids = new std::vector<std::string>(); ids->push_back(*$1); $$ = ids; delete $1;} 

method_list: {decafStmtList * slist = new decafStmtList(); $$ = slist; }
    | method_list MethodDecl { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($2); $$ = $1;}
    
typed_var_defs:
    { decafStmtList* slist = new decafStmtList(); $$ = slist; }
    | typed_var_defs T_COMMA T_ID normal_type { decafStmtList * slist = (decafStmtList *)$1; VarDefAST * def = new VarDefAST(*$3, ( TypeAST *)$4); slist->push_back(def); delete $3; $$ = $1;}
    | T_ID normal_type { decafStmtList* slist = new decafStmtList(); VarDefAST * def = new VarDefAST(*$1, ( TypeAST *)$2); slist->push_back(def); delete $1; $$ = slist;   }

MethodDecl: T_FUNC T_ID T_LPAREN typed_var_defs T_RPAREN return_type MethodBlock
    {
        MethodAST * method_decl = new MethodAST(*$2, (TypeAST *)$6, (decafStmtList *)$4, (MethodBlockAST *)$7);
        delete $2;
        $$ = method_decl;
    }

MethodBlock: T_LCB VarDecls Statements T_RCB 
    {
        MethodBlockAST * block = new MethodBlockAST((decafStmtList *)$2, (decafStmtList *)$3);
        $$ = block;
    }

Block: T_LCB VarDecls Statements T_RCB 
    {
        BlockAST * block = new BlockAST((decafStmtList *)$2, (decafStmtList *)$3);
        $$ = block;
    }
  
Statements: {decafStmtList * slist = new decafStmtList(); $$ = slist;}
    | Statements Statement { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($2); $$ = slist; }

ElseBlock:
    { $$ = nullptr; }
    | T_ELSE Block { $$ = $2; }

Statement: Block { $$ = $1; }
    | Assign T_SEMICOLON { $$ = $1; }
    | MethodCall T_SEMICOLON { $$ = $1; }
    | T_IF T_LPAREN Expr T_RPAREN Block ElseBlock { IfStmtAST * ifast = new IfStmtAST((ExprAST *)$3, (BlockAST *)$5, (BlockAST *)$6 );$$ = ifast; }
    | T_WHILE T_LPAREN Expr T_RPAREN Block { WhileStmtAST * whileast = new WhileStmtAST((ExprAST *)$3, (BlockAST *)$5); $$ = whileast; }
    | T_FOR T_LPAREN ForAssignList T_SEMICOLON Expr T_SEMICOLON ForAssignList T_RPAREN Block { ForStmtAST * forast = new ForStmtAST((decafStmtList *)$3, (ExprAST *)$5, (decafStmtList *)$7, (BlockAST *)$9); $$ = forast; }
    | T_RETURN T_LPAREN Expr T_RPAREN  T_SEMICOLON { ReturnStmtAST * returnast = new ReturnStmtAST((ExprAST *)$3); $$ = returnast; }
    | T_RETURN T_LPAREN T_RPAREN T_SEMICOLON {ReturnStmtAST * returnast = new ReturnStmtAST(nullptr); $$ = returnast; }
    | T_RETURN T_SEMICOLON { ReturnStmtAST * returnast = new ReturnStmtAST(nullptr); $$ = returnast; }
    | T_BREAK T_SEMICOLON { BreakStmtAST * breakast = new BreakStmtAST(); $$ = breakast; }
    | T_CONTINUE T_SEMICOLON { ContinueStmtAST * cntast = new ContinueStmtAST(); $$ = cntast; }

Assign: 
    T_ID T_ASSIGN Expr
        {
            AssignVarAST * assign = new AssignVarAST(*$1, (ExprAST *)$3);
            delete $1;
            $$ = assign;
        }
    | T_ID T_LSB Expr T_RSB T_ASSIGN Expr
        {
            AssignArrayLocAST * arr_loc = new AssignArrayLocAST(*$1, (ExprAST *)$3, (ExprAST *)$6);
            delete $1;
            $$ = arr_loc;
        }

MethodArg: Expr { $$ = $1;}

MethodArgList: { decafStmtList * slist = new decafStmtList(); $$ = slist; }
    | MethodArgList T_COMMA MethodArg {decafStmtList * slist = (decafStmtList *)$1; slist->push_back($3); $$ = $1;}
    | MethodArg { decafStmtList * slist = new decafStmtList(); $$ = slist; slist->push_back($1); } 

MethodCall: T_ID T_LPAREN MethodArgList T_RPAREN
    {
        MethodCallAST * call = new MethodCallAST(*$1, (decafStmtList *)$3); 
        $$ = call;
        delete $1;
    }

Expr:
    T_ID { VariableExprAST * var_exp = new VariableExprAST(*$1); delete $1; $$ = var_exp; }
    | MethodCall { $$ = $1; }
    | constant_expr { $$ = $1; }
    | T_LPAREN Expr T_RPAREN { $$ = $2 }
    | T_ID T_LSB Expr T_RSB {  ArrayLocExprAST * arr = new ArrayLocExprAST(*$1, (ExprAST *)$3); $$ = arr; delete $1; }
    | Expr T_PLUS Expr { BinaryExprAST * binexp = new BinaryExprAST("Plus", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_MINUS Expr {BinaryExprAST * binexp = new BinaryExprAST("Minus", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_MULT Expr { BinaryExprAST * binexp = new BinaryExprAST("Mult", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_DIV Expr { BinaryExprAST * binexp = new BinaryExprAST("Div", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_LEFTSHIFT Expr { BinaryExprAST * binexp = new BinaryExprAST("Leftshift", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_RIGHTSHIFT Expr { BinaryExprAST * binexp = new BinaryExprAST("Rightshift", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_MOD Expr { BinaryExprAST * binexp = new BinaryExprAST("Mod", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_LT Expr { BinaryExprAST * binexp = new BinaryExprAST("Lt", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_GT Expr { BinaryExprAST * binexp = new BinaryExprAST("Gt", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_LEQ Expr { BinaryExprAST * binexp = new BinaryExprAST("Leq", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp; }
    | Expr T_GEQ Expr { BinaryExprAST * binexp = new BinaryExprAST("Geq", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp;}
    | Expr T_EQ Expr { BinaryExprAST * binexp = new BinaryExprAST("Eq", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp;}
    | Expr T_NEQ Expr { BinaryExprAST * binexp = new BinaryExprAST("Neq", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp;}
    | Expr T_AND Expr { BinaryExprAST * binexp = new BinaryExprAST("And", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp;}
    | Expr T_OR Expr { BinaryExprAST * binexp = new BinaryExprAST("Or", (ExprAST *)$1, (ExprAST *)$3); $$ = binexp;}
    | T_MINUS Expr %prec UMINUS { UnaryExprAST * unary = new UnaryExprAST("UnaryMinus", (ExprAST *)$2); $$ = unary;}
    | T_NOT Expr %prec UNOT { UnaryExprAST * unary = new UnaryExprAST("Not", (ExprAST *)$2); $$ = unary;}


ForAssignList: /* { Assign }+, */
    { decafStmtList * slist = new decafStmtList(); $$ = slist}
    | ForAssignList T_COMMA Assign { decafStmtList * slist = (decafStmtList *)$1; slist->push_back($3); $$ = slist; }
    | Assign { decafStmtList * slist = new decafStmtList(); slist->push_back($1); $$ = slist; }


%%

int main() {
  // parse the input and create the abstract syntax tree
  int retval = yyparse();
  return(retval >= 1 ? EXIT_FAILURE : EXIT_SUCCESS);
}

