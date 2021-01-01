%{
#include <stdio.h>
%}

%%
"/*"([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+\/ {}
"//".*		{ }
\"(.)*\" ECHO;
.|\n	ECHO;
%%

int main(int k,char **argcv) 
{ 
    yylex(); 
    return 0;
}