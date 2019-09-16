%{
    #include <stdio.h>
    #include <stdlib.h>

    #define INT 1
    #define BIN 2
    #define HEX 3
    #define OCT 4

    #define integer_t 1
%}

%start TOML

%%
    TOML: TABLE | ARRAY ;
    DATA: [INT_KEY | BIN_KEY | HEX_KEY | OCT_KEY | STRING_KEY];
    INT_KEY: INTEGER;
    BIN_KEY: BIN_HEADER | BINARY;
    HEX_KEY: HEX_HEADER | BINARY;
    OCT_KEY: OCT_HEADER | BINARY;
    STRING_KEY: [STRING | MULTI_LINE_STRING];
    FLOAT_KEY: FLOAT;
    TABLE_NAME: [LETTER | STRING] | [DOT] | [LETTER | STRING];
    TABLE: LBRACKET 
         | TABLE_NAME 
         | RBRACKET 
         | NEWLINE 
         | [KEY]+
         {
            parse_table();
         }
         ;
    ARRAY: LBRACKET 
         | DATA 
         | [COMMA | DATA]+ 
         | RBRACKET
         {
            parse_array();
         }
         ;
    KEY: NAME 
       | EQUAL 
       | DATA
       {
           int type;
           switch(DATA){
                case INT_KEY:
                    type = integer_t;
                    break;
                case BIN_KEY:
                    break;
           }
           char temp;
           load_key(TABLE,NAME,temp,type);
       }
       ;

    INLINE_TABLE: LBRACE | KEY | RBRACE;
%%

int yyerror(const char* str){
    extern char* yytext;
    fprintf(stderr, "Parser Libary Error!!");
    return 0;
}

int parse_array(){

}

int parse_table(){

}

int load_key(const char* key_name,const char* tbl_name,const char* rtn_key,int* val_type){
    return 0;
}
