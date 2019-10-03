%{
    #include <stdio.h>
    #include <string.h>
    // Original yacc
    #include "y.tab.h"
    // Bison
    #include "toml.tab.h"
    int yywarp(void) {return 1;}
    #define TRUE 1
    #define FALSE 0
    static char* buf[255];
    static char* string_literal;
    void string_free(void){
        string_literal = 0;
    }
%}

digit       [0-9]+[_]*;
integer     [+-\-]?digit;
floating    [+-\-]digit\.[e]?[];
letter      [a-zA-Z]
binary      [0-9a-fA-F]
date        [[0-9][0-9]{1,4}][\-][0-1][0-9][\-][0-3][0-9]
time        [0-2][0-9][:][0-5][0-9][:][0-5][0-9][.][0-9]{0,6}
/* Example:1999-05-28T14:21:00.000000*/
timestamp  date?[T]time?

%%
<INITIAL>#              BEGIN COMMENT;
<INITIAL>"\""           return QUOTE;
<INITIAL>"\f"           return FORM_FEED;
<INITIAL>"\'"           return DOUBLEQUOTE;
<INITIAL>"\0"           return NUL;
<INITIAL>"."            return DOT;
<INITIAL>","            return COMMA;
<INITIAL>"="            return EQUAL;
<INITIAL>"{"            return LBRACE;
<INITIAL>"}"            return RBRACE;
<INITIAL>"\r"           BEGIN CRETURN
<INITIAL>"\n"           return NEWLINE;
<INITIAL>"["            return LBRACKERT;
<INITIAL>"]"            return RBRACKERT;
<INITIAL>"-"            return HYPHEN;
<INITIAL>":"            return CORON;
<INITIAL>"0b"           return BIN_HEADER;
<INITIAL>"0x"           return HEX_HEADER;
<INITIAL>"0o"           return OCT_HEADER;
<INITIAL>"true"         return BOOL_TRUE;
<INITIAL>"false"        return BOOL_FALSE;
<INITIAL>integer        return INTEGER;
<INITIAL>binary         return BINARY;
<INITIAL>floating       return DOUBLE;
<INITIAL>timestamp      return TIMESTAMP;
<INITIAL>\"             {
    BEGIN WQUOTE_STRING;
    string_literal = buf;
}
<INITIAL>\'             {
    BEGIN QUOTE_STRING;
    string_literal = buf;
}
<INITIAL>\"\"\"         {
    BEGIN WQUOTE_MULTI_LINE;
    string_literal = buf;
}
<INITIAL>\'\'\'         { 
    BEGIN QUOTE_MULTI_LINE;
    string_literal = buf;
}

<CRETURN>\n {
    return NEWLINE;
    BEGIN INITIAL;
}
 // Comment Rule
<COMMENT>"\n" BEGIN INITIAL;
<COMMENT>"\r\n" BEGIN INITIAL;
<COMMENT>. ;

<WQUOTE_STRING>\" {
    string_free()
    BEGIN INITIAL;
    return STRING;
}
<WQUOTE_STRING>\' string_literal++ = *yytext[0];
<WQUOTE_STRING>. string_literal++ = *yytext[0];
<WQUOTE_STRING>\\\r string_literal++ = '\r'
<WQUOTE_STRING>\\\n string_literal++ = '\n'
<WQUOTE_STRING>\\\" string_literal++ = '\"'

<QUOTE_STRING>\\\r string_literal++ = '\r'
<QUOTE_STRING>\\\n string_literal++ = '\n'
<QUOTE_STRING>\\\' string_literal++ = 
<QUOTE_STRING>\' {
    string_free()
    BEGIN INITIAL;
    return STRING;
}
<QUOTE_STRING>. string_literal++ = *yytext[0];

<WQUOTE_MULTI_LINE>\"\"\" {
    string_free()
    BEGIN INITIAL;
    return STRING;
} 
%%