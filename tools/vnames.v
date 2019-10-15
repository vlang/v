module main

import os
import compiler

fn main(){
	
	//User input setup:
	mut file := os.realpath( os.args[1] )
	
	// Preparation for the compiler module:
	// VEXE env variable is needed so that compiler.vexe_path()
	// can return it later to whoever needs it:
	os.setenv('VEXE', '/v/nv/v', true)

	// main work:
	vexe := compiler.vexe_path()
	mut v := compiler.new_v([vexe,file])
	v.add_v_files_to_compile()
	for f in v.files { v.parse(f, .decl) }
	fi := v.get_file_parser_index( file ) or { panic(err) }
	fmod :=  v.parsers[fi].mod 
	
	// output:
	mut fns :=[]string
	for _, f in v.table.fns {
		if !f.is_public { continue }
		if fmod != f.v_fn_module() { continue }
		if fmod == 'builtin' { fns << f.v_fn_name() continue }
		fns << f.v_fn_module() + '.' + f.v_fn_name()
	}
	fns.sort()
	for f in fns { println(f) }
	
}
