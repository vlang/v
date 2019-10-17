// This is Bison/Flex Warpper.
// TOML parser create by Bison/Flex.

import TOML

struct C.toml_key_t{
	key 		byteptr
	kind_type 	int
	val_type 	int
	num_keyval 	int
	val &byteptr
    arr &&C.array_t
    tab &&C.table_t
}

struct C.array_t{
	key byteptr
	num_keyval int
	keyval &&C.toml_key_t
}

struct C.table_t{

}

fn compile(){
	lex_compile(toml.l)
	yacc_compile(toml.y)
}

fn writing_data(){

}

fn integer_writing(){

}

fn string_writing(){

}