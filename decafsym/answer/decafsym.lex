%{
#include "default-defs.h"
#include "decafsym.tab.h"
#include <cstring>
#include <string>
#include <sstream>
#include <iostream>
using namespace std;
extern int yylineno;
extern int yytokenpos;
string & covert_newline(string & s){
    string tmp = "";
    for(size_t i = 0; i < s.size(); i++)
      if(s[i] == '\n')
        tmp += "\\n";
      else
        tmp += s[i];
    s = tmp;
    return s;
}

%}
%option yylineno
%%
  /*
    Pattern definitions for all tokens1
  */
\{                         { return T_LCB; }
\}                         { return T_RCB; }
bool                       { yylval.sval = new string("BoolType"); return T_BOOLTYPE; }
package                    { return T_PACKAGE; }
func                       { return T_FUNC; }
return                     { return T_RETURN; }
while                      { return T_WHILE; }
void                       { yylval.sval = new string("VoidType");return T_VOID; }
var                        { return T_VAR; }
string                     { yylval.sval = new string("StringType");return T_STRINGTYPE; }
true                       { return T_TRUE; }
null                       { return T_NULL; }
int                        { yylval.sval = new string("IntType");return T_INTTYPE; }
if                         { return T_IF; }
extern                     { return T_EXTERN; }
for                        { return T_FOR; }
break                      { return T_BREAK; }
continue                   { return T_CONTINUE; }
else                       { return T_ELSE; }
false                      { return T_FALSE; }
[a-zA-Z\_][a-zA-Z\_0-9]*   { yylval.sval = new string(yytext); return T_ID; }
,                          { return T_COMMA; }
==                         { return T_EQ;  }
>=                         { return T_GEQ; }
>                          { return T_GT;  }
\<\<                       { return T_LEFTSHIFT;  }
>>                         { return T_RIGHTSHIFT; }
\<=                        { return T_LEQ; }
\[                         { return T_LSB; }
\]                         { return T_RSB; }
\<                         { return T_LT; }
\-                         { return T_MINUS; }
\+                         { return T_PLUS; }
\%                         { return T_MOD; }
\*                         { return T_MULT; }
!=                         { return T_NEQ; }
!                          { return T_NOT; }
\|\|                       { return T_OR; }
;                          { return T_SEMICOLON; }
\"([^\n"\\]|\\(a|b|t|n|v|f|r|\\|\'|\"))*\" {  yylval.sval = new string(yytext); return T_STRINGCONSTANT; }

([0-9]+(\.[0-9]+)?)|(0[xX][0-9A-Fa-f]+)          { yylval.sval = new string(yytext); return T_INTCONSTANT; }
\'([^\n'\\]|\\(a|b|t|n|v|f|r|\\|\'|\"))\'        { yylval.sval = new string(yytext); return T_CHARCONSTANT; }
"//".*"\n"                 {  }
\(                         { return T_LPAREN; }
\)                         { return T_RPAREN; }

&&                         { return T_AND; }
=                          { return T_ASSIGN; }
\/                         { return T_DIV; }
"."                        { return T_DOT; } 
[\t\r\n\a\v\b ]+           {  } /* ignore whitespace */
.                          { cerr << "Error: unexpected character in input" << endl; return -1; }
%%

int yyerror(const char *s) {
  cerr << yylineno << ":" << s << endl;
  return 1;
}

