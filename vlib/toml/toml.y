%{
    #include <stdio.h>
    #include <stdlib.h>

    #define STR_DATA 1
    #define DEC_INT 2
    #define BIN_INT 3
    #define HEX_INT 4
    #define OCT_INT 5
    #define FLOAT_NUM 6
    #define TIME_STAMP 7
%}

%start TOML

%%
    TOML: TABLE | ARRAY ;
    DATA: [INT_KEY | BIN_KEY | HEX_KEY | OCT_KEY | STRING_KEY];
    DEC_KEY: 
           |INTEGER;
           {
              parse_int(1,INTEGER); 
           }
    BIN_KEY:
           | BIN_HEADER 
           | BINARY;
           {
               parse_int(2,BINARY)
           }
    HEX_KEY:
           | HEX_HEADER 
           | BINARY;
           {
               parse_int(3,BINARY)
           }
    OCT_KEY:
           | OCT_HEADER 
           | BINARY;
           {
               parse_int(4,BINARY)
           }
    STRING_KEY: 
              | [STRING | MULTI_LINE_STRING];
    FLOAT_KEY: FLOAT;
    TABLE_OF_ARRAY:LBRACKET|LBRACKET|TABLE_NAME|RBRACKET|RBRACKET ;
    TABLE_NAME:
              | [LETTER | STRING] 
              | [DOT] 
              | [LETTER | STRING];
    TABLE: LBRACKET 
         | TABLE_NAME 
         | RBRACKET 
         | NEWLINE 
         | [KEY]+
         {
            parse_table(TABLE_NAME);
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
    const char* key;
    const char* val;
};

struct table_t{
    const char* key;
    int         kind;
    int         type;

    int num_keyval;
    union {
        char**    val;
        array_t** arr;
        table_t** tab;
    } u;
};

struct array_t{
    const char* key;

    int          nkval;
    toml_key_t** kval;

    int     narr;
    array_t arr;

    int     ntab;
    table_t tab;
};

// 1:decmical, 2:binary, 3:hexical, 4:octal
int parse_int(int head,int data)

int parse_array(const char* key_name,array_t rtn){

}

int parse_table(const char* tbl_name,table_t rtn){

}

int load_key(const char* key_name,const char* tbl_name,const char* rtn_key,int* val_type){
    return 0;
}
