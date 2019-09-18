%{
    #include <stdio.h>
    #include <stdlib.h>

    #define STRING 1
    #define DEC_INT 2
    #define BIN_INT 3
    #define HEX_INT 4
    #define OCT_INT 5
    #define FLOAT_NUM 6
%}

%start TOML

%%
    TOML: TABLE | ARRAY ;
    DATA: [INT_KEY | BIN_KEY | HEX_KEY | OCT_KEY | STRING_KEY];
    DEC_KEY: INTEGER;
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

           // Key Type Select.
           switch(DATA){
                case STRING_KEY:
                    type = STRING
                    break;
                case DEC_KEY:
                    type = DEC_INT;
                    break;
                case BIN_KEY:
                    type = BIN_INT;
                    break;
                case HEX_KEY:
                    type = HEX_INT;
                    break;
                case OCT_KEY:
                    type = OCT_INT;
                    break;
                case FLOAT_KEY:
                    type = FLOAT;
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

struct toml_key_t{
    int val_type;
};

struct table_t{
    const char* key;
    int num_keyval;
};

struct array_t{
    const char* key;
};

int parse_array(const char* key_name,array_t rtn){

}

int parse_table(const char* tbl_name,table_t rtn){

}

int load_key(const char* key_name,const char* tbl_name,const char* rtn_key,int* val_type){
    return 0;
}
