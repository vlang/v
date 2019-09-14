%{
    #include <stdio.h>
    #include <string.h>
    #include "toml.tab.h"
    int yywarp(void) {return 1;}
%}

digit       [0-9]+[_]*
integer     [+-\-]?digit
floating    [+-\-]digit\.[e]?[]
letter      [a-zA-Z]
binary      [0-9a-fA-F]

/* Example:1999-05-28T14:21:00.000000*/
timestamp  

%%
    "\""        return QUOTE;
    "\'"        return DOUBLEQUOTE;
    "\0"        return NUL;
    "."         return DOT;
    ","         return COMMA;
    "="         return EQUAL;
    "{"         return LBRACE;
    "}"         return RBRACE;
    "\n"        return NEWLINE;
    "["         return LBRACKERT;
    "]"         return RBRACKERT;
    "-"         return HYPHEN;
    ":"         return CORON;
    "0b"        return BIN_HEADER;
    "0x"        return HEX_HEADER;
    "0o"        return OCT_HEADER;
    true        return BOOL_TRUE;
    false       return BOOL_FALSE;
    integer     return INTEGER;
    binary      return BINARY;
    floating    return FLOAT;

    "\"[^\"]"* {
        if(yytext[yyleng-2] == '\\'){
            yymore();
        }
        else if(yytext[yyleng-4] == '\"'){
            return ERROR;
        }
        else{
            return STRING;
        }
    }

    /*Multi Line String,.*/
    "\"\"\"[^\"\"\"]"*{
        if(yytext[yyleng-2] == '\\'){
            yymore();
        }
        else{
            return MULTI_LINE_STRING;
        }
    }
    
%%