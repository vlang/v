%{
    #include <stdio.h>
    #include <string.h>
    #include "toml.tab.h"
    int yywarp(void) {return 1;}
%}

floating [0-9]+\.[0-9]+
integer [0-9]+
letter [a-zA-Z]

%%
    "\"" return QUOTE;
    "\'" return DOUBLEQUOTE;
    "\0" return NUL;
    "." return DOT;
    "," return COMMA;
    "=" return EQUAL;
    "{" return LBRACE;
    "}" return RBRACE;
    "\n" return NEWLINE;
    "[" return LBRACKERT;
    "]" return RBRACKERT;
    \"[^"]* {
        if(yytext[yyleng-2] == '\\'){
            yymore();
        }
        else{
            return string
        }
    }
    
%%