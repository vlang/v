module main

import os
import compiler

fn main(){
	
	//Setup of input:
	file := os.realpath( os.args[1] )
	//println(file)
	//	file = '/v/nv/vlib/bitfield/bitfield.v'
	//	file = '/v/nv/vlib/http/http.v'
	//file = '/v/nv/vlib/builtin/int.v'
	
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
	//println('pindex: $fi | fmod: $fmod')
	
	// output:
	//println('Imports: ' + v.table.imports.str())
	//println('Modules: ' + v.table.modules.str())
	//println('Functions: ')
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
