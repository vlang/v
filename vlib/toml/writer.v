// This is Bison/Flex Warpper.
// TOML parser create by Bison/Flex.

import TOML

fn load_parse(){
	&C.yyparse()
}

struct C.toml_key_t{
	val_type int
}

struct C.array_t{

}

struct C.table_t{

}

fn writing_data(){

}

fn integer_writing(){

}

fn string_writing(){

}