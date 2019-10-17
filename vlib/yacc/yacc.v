// yacc header include.
#include "y.tab.h"

fn &C.yyparse()

fn yacc_compile(file string) {
	// yacc or bison compile for C
	yacc := system('bison -y $file') or system('yacc $file') or eprintln('Please Install yacc/bison.')
}

fn bison_default_compile(file string){
    bison := system('bison $file') or eprintln('Bison not installed.')
}

fn lex_compile(file string) {
    // lex or flex compile for C
	lex := system('flex -d $file') or system('lex -d $file') or eprintln('Please Install lex/flex.')
}

fn load_parse(){
	&C.yyparse()
}
