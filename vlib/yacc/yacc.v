// yacc header include.
#include "y.tab.h"

fn &C.yyparse()

struct YaccConfig{
	output_name string: none
}

struct BisonConfig {

}

struct LexConfig {

}

struct FlexConfig{
	output_name string: none
}

fn yacc_compile(file string,config YaccConfig) {
	// yacc or bison compile for C
	yacc := system('bison -y $file') or system('yacc $file') or eprintln('Please Install yacc/bison.')
}

fn bison_compile(file string) {
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
