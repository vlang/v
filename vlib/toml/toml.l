%{
    #include <stdio.h>
    #include <string.h>
    // Original yacc
    #include "y.tab.h"
    // Bison
    #include "toml.tab.h"
    int yywarp(void) {return 1;}
    char* buffer;
    extern FILE* yyin;
%}

digit       [0-9]+[_]*;
integer     [+-\-]?digit;
floating    [+-\-]digit\.[e]?[+-\-]?[digit];
letter      [a-zA-Z];
binary      [0-9a-fA-F];
date        [0-9]+[\-][0-1][0-9][\-][0-3][0-9];
time        [0-2][0-9][:][0-5][0-9][:][0-5][0-9][.][0-9]{0,6};
/* Example:1999-05-28T14:21:00.000000*/
timestamp  date?[T]time?;

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
<INITIAL>\"             BEGIN WQUOTE_STRING;
<INITIAL>\'             BEGIN QUOTE_STRING;
<INITIAL>\"\"\"         BEGIN WQUOTE_MULTI_LINE;
<INITIAL>\'\'\'         BEGIN QUOTE_MULTI_LINE;
<CRETURN>\n {
    return NEWLINE;
    BEGIN INITIAL;
}
 // Comment Rule
<COMMENT>"\n" BEGIN INITIAL;
<COMMENT>"\r\n" BEGIN INITIAL;
<COMMENT>. ;

<WQUOTE_STRING>\" {
    BEGIN INITIAL;
    return STRING;
}
<WQUOTE_STRING>\' yy_scan_buffer("\'");
<WQUOTE_STRING>. yy_scan_buffer(*yytext[0]);
<WQUOTE_STRING>\n yyerror("Error: Invalid String.");
<WQUOTE_STRING>\\\r yy_scan_buffer("\r");
<WQUOTE_STRING>\\\n yy_scan_buffer("\n");
<WQUOTE_STRING>\\\" yy_scan_buffer("\"");
<WQUOTE_STRING>\\\\ yy_scan_buffer("\\");
<WQUOTE_STRING>\" {
    BEGIN INITIAL;
    return STRING;
}

<QUOTE_STRING>\"  yy_scan_buffer("\"");
<QUOTE_STRING>\n yyerror("Error: Invalid String.")
<QUOTE_STRING>. yy_scan_buffer(*yytext[0]);
<QUOTE_STRING>\\\r yy_scan_buffer("\r");
<QUOTE_STRING>\\\n yy_scan_buffer("\n");
<QUOTE_STRING>\\\' yy_scan_buffer("\'"):
<QUOTE_STRING>\\\\ yy_scan_buffer("\\")
<QUOTE_STRING>\' {
    BEGIN INITIAL;
    return STRING;
}

<QUOTE_MULTI_LINE>\'\'\' {
    BEGIN INITIAL;
    return STRING;
}
<WQUOTE_MULTI_LINE>\"\"\" {
    BEGIN INITIAL;
    return STRING;
} 
%%