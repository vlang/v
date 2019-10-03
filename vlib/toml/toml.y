%{
    #include <stdio.h>
    #include <stdlib.h>
    #define YYDEBUG 1
    struct toml_key_t;
    struct toml_table_t;
    struct toml_array_t;
    struct toml_timestamp_t;
    // 's'tring,'d'emical,'b'inary,'h'exical,'o'ctial,do'u'ble,'t'imestamp,'n'one
    static int parse_data(int val_type,char* data);
    int parse_int(char* rtn,int val_type);
    int parse_array(const char* key_name,array_t rtn);
    int parse_table(const char* tbl_name,table_t rtn);
    int load_key(const char* key_name,const char* tbl_name,const char* rtn_key,int val_type)
%}

%token TABLE DATA DEC_KEY BIN_KEY HEX_KEY OCT_KEY STRING_KEY
       FLOAT
%start TOML

%%
    TOML: TABLE | ARRAY ;
    DATA: [INT_KEY | BIN_KEY | HEX_KEY | OCT_KEY | STRING_KEY | DOUBLE];
    DEC_KEY: 
           | INTEGER;
           {
                parse_data('d',INTEGER); 
           }
    BIN_KEY:
           | BIN_HEADER 
           | BINARY;
           {
                parse_data('b',BINARY);
           }
    HEX_KEY:
           | HEX_HEADER 
           | BINARY;
           {
                parse_data('h',BINARY);
           }
    OCT_KEY:
           | OCT_HEADER 
           | BINARY;
           {
                parse_data('o',BINARY);
           }
    STRING_KEY: 
              | STRING;
              {
                    parse_data('s',STRING);
              }
    DOUBLE_KEY: 
              | DOUBLE;
              {
                    parse_data('u',DOUBLE);
              }
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
        int type
           // Key Type Select.
           switch(DATA){
                case STRING_KEY:
                    type = 's';
                    break;
                case DEC_KEY:
                    type = 'd';
                    break;
                case BIN_KEY:
                    type = 'b';
                    break;
                case HEX_KEY:
                    type = 'h';
                    break;
                case OCT_KEY:
                    type = 'o';
                    break;
                case DOUBLE_KEY:
                    type = 'u';
                    break;
                case TIMESTAMP_KEY;
                    type = 't';
           }
           char temp;
           load_key(NAME,TABLE_NAME,temp,type);
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
} toml_key;

struct toml_table_t{
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

struct toml_array_t{
    const char* key;

    int          nkval;
    toml_key_t** kval;

    int     narr;
    toml_array_t arr;

    int     ntab;
    toml_table_t tab;
};

int parse_data(int val_type,char* data){

}

int parse_int(char* rtn_key,int val_type){

}

int parse_string(){

} 

int parse_array(const char* key_name,array_t rtn){

}

int parse_table(const char* tbl_name,table_t rtn){

}

int load_key(const char* key_name,const char* tbl_name,const char* rtn_key,int val_type){
    if(val_type == 'd' || val_type == 'b' || val_type == 'h' || val_type == 'o'){
        parse_int(rtn_key,val_type)
    }
    return 0;
}
