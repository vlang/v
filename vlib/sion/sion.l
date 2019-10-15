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