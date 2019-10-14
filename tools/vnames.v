module main

import os
import compiler

fn main(){
	os.setenv('VEXE', '/v/nv/v', true)
	mut file := os.args.first()
	file = '/v/nv/vlib/benchmark/benchmark.v'
	vexe := compiler.vexe_path()
	mut v := compiler.new_v([vexe,file])
	{
		v.add_v_files_to_compile()
		for f in v.files { v.parse(f, .decl) }
	}
	{
		println('Imports: ' + v.table.imports.str())
		println('Modules: ' + v.table.modules.str())
		println('Functions: ')
		for _, f in v.table.fns {		
			println('${f.v_fn_module()}.${f.v_fn_name()}')
		}
	}
}
