%{
    #include<stdio.h>
    #define YYDEBUG 1
    void yyerror(char* message){
        printf("Error: %s\n",message);
    }
%}