module main

import os
import flag
import compiler

const (
	tool_version = '0.0.1'
	tool_description = '  Extracts the function names declared in a v file.'
)

fn f_to_string(fmod string, f compiler.Fn) ?string {
	svisibility := if f.is_public {
		'public'
	}else{
		'private'
	}
	if fmod != f.v_fn_module() { return none }
	if fmod == 'builtin' {
		return '$svisibility\t' + f.v_fn_name()
	}
	return '$svisibility\t' + f.v_fn_module() + '.' + f.v_fn_name()
}

fn analyze_v_file(file string) {
	println('')
	println('######################  $file  ######################')
	
	// main work:
	vexe := compiler.vexe_path()
	println(vexe)
	println(file)
	mut v := compiler.new_v([vexe,file])
	v.add_v_files_to_compile()
	for f in v.files { v.parse(f, .decl) }
	fi := v.get_file_parser_index( file ) or { panic(err) }
	fmod :=  v.parsers[fi].mod 
	
	// output:
	mut fns :=[]string
	for _, f in v.table.fns {
		fname := f_to_string(fmod, f) or { continue }
		fns << fname
	}
	fns.sort()
	for f in fns { println(f) }
	
}

fn no_js_v_files(val string, index int, arr []string) bool {
	if val.ends_with('_js.v'){ return false }
	return true
}
	
fn main(){
	// Preparation for the compiler module:
	// VEXE env variable is needed so that compiler.vexe_path()
	// can return it later to whoever needs it:
	toolexe := os.executable()
	os.setenv('VEXE', os.dir(os.dir(toolexe)) + '/v', true)	
	
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.filename(toolexe))
	fp.version( tool_version )
	fp.description( tool_description )
	fp.arguments_description('FILE.v/FOLDER [FILE.v/FOLDER]...')
	fp.limit_free_args_to_at_least(1)
	fp.skip_executable()
	show_help:=fp.bool_('help', `h`, false, 'Show this help screen\n')	
	if( show_help ){
		println( fp.usage() )
		exit(0)
	}
	
	mut files := []string
	locations := fp.finalize() or { eprintln('Error: ' + err) exit(1) }
	for xloc in locations {
		loc := os.realpath(xloc)
		if os.is_dir(loc){
			files << os.walk_ext(loc,'.v').filter(no_js_v_files)
		}else{
			files << [ loc ].filter(no_js_v_files)
		}
	}

	println('Files: ' + files.str())
	for file in files {
		analyze_v_file(file)
	}
}
