%{
#include <stdio.h>
#include <stdbool.h>
int symtbl[26];
bool issym[26];

int yylex(void);
int yyerror(char *);
%}

%union {
  int rvalue; /* value of evaluated expression */
  int lvalue; /* index into symtbl for variable name */
}

%token <rvalue> NUMBER
%token <lvalue> NAME 
%left '+' '-'
%left '*' '/'
%left UMINUS

%type <rvalue> expression
%type <rvalue> realNum




%%
statementList: statement|statementList statement;
statement: NAME '=' expression { symtbl[$1] = $3; issym[$1] = true; printf("%c = %d\n", $1 + 'a', $3); }
         | expression  { printf("%d\n", $1);  }
         ;

expression:realNum
         | expression '+' expression { $$ = $1 + $3; }
         | expression '-' expression { $$ = $1 - $3; }
         | expression '*' expression { $$ = $1 * $3; }
         | expression '/' expression { $$ = $1 / $3; }
         | '-' expression %prec UMINUS { $$ = -$2; }
         | '(' expression ')' { $$ = $2; } 
         ;

realNum: NUMBER { $$ = $1; }
      | NAME {
        if (!issym[$1]){
          printf("Undefined symbol:%c \n", 'a' + $1);
        }else{
          $$ = symtbl[$1];
        }
      }
%%


