// yacc header include.
#include "y.tab.h"

fn &C.yyparse()

struct YaccConfig{
	file_prefix:	''	  // -b
	define_name:	''	  // -d
	prefix_name:	''	  // -p
	debug_mode: 	false // -t
	report: 		false // -v
	optimize:		false // -l
	parser_only:	false // -n
 }

struct BisonConfig {
	file_prefix:	''	  // -b
	define_name:	''	  // -d
	output_name: 	''	  // -o
	prefix_name:	''	  // -p
	debug_mode: 	false // -t
	report: 		false // -v
	optimize:		false // -l
	parser_only:	false // -n
}

struct LexConfig {
	debug_mode: 	false
	report: 		false
	optimize:		false
}

struct FlexConfig{
	output_name: 	''    // -o
	debug_mode: 	false
	report: 		false
	optimize:		false
	intractive:		false
	trace_mode:		false
	input:			false // -8
}

fn yacc_config(){

}

fn flex_config(){

}

fn yacc_compile(file_path string,config YaccConfig) {
	args := yacc_config(config)
	// yacc or bison compile for C
	yacc := system('bison -y $file_path $args') or system('yacc $file_path $args') or eprintln('Please Install yacc/bison.')
}

fn bison_compile(file string, config BisonConfig) {
	args := bison_config(config)
	// bison default option compile for C
    bison := system('bison $file') or eprintln('Bison not installed.')
}

fn lex_compile(file string) {
    // lex or flex compile for C
	lex := system('flex $file') or system('lex $file') or eprintln('Please Install lex/flex.')
}

fn flex_compile(file string){
	flex := system('flex $file') or eprintln('Flex not installed.')
}

fn load_parse(){
	&C.yyparse()
}
