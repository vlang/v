%{
    #include <stdio.h>
    #include <string.h>
    #define INT 1
    #define BIN 2
    #define HEX 3
    #define OCT 4
%}

%start TOML

%%
    TOML: TABLE | ARRAY ;
    KEY: NAME | EQUAL | DATA;
    INT_KEY: INTEGER;
    HEX_KEY: HEX_HEADER | BINARY;
    OCT_KEY: OCT_HEADER | BINARY;
    STRING_KEY: [STRING | MULTI_LINE_STRING];
    FLOAT_KEY: FLOAT;
    TABLE_NAME: [LETTER | STRING] | [DOT] | [LETTER | STRING];
    TABLE: LBRACKET | TABLE_NAME | RBRACKET | NEWLINE | [KEY]+;
    ARRAY: LBRACKET | DATA | [COMMA | DATA]+ | RBRACKEY;
%%

int token_string(const char* key_name,const char* rtn_val){
    // invalid key name check.
    if(key_name != KEY.NAME){
        return -1;
    }
    rtn_val = KEY.DATA;
    return 0;
}

int token_integer(const char* key_name,int header,const char* rtn_val){
    // invalid key name check.
    if(key_name != KEY.NAME){
        return -1;
    }
    // binary header check.
    switch(){
        case INTEGER:
            header = INT;
            break;
        case BIN_KEY:
            header = BIN;
            break
        case HEX_KEY:
            header = HEX;
            break;
        case OCT_KEY:
            header = OCT;
            break;
        default:
            return -1
    }
    rtn_val = DATA;
    return 0;
}

