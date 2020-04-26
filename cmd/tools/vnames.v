module main

/*
QTODO
import os
import flag
import strings
import compiler
import v.pref

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
	hash := strings.repeat(`#`, (76 - file.len) / 2)
	println('$hash  $file  $hash')

	// main work:
	mut pref := &pref.Preferences{
		path: file
	}
	pref.fill_with_defaults()
	mut v := compiler.new_v(pref)
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

fn main(){
	toolexe := os.executable()
	compiler.set_vroot_folder(os.dir(os.dir(os.dir(toolexe))))

	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(toolexe))
	fp.version( tool_version )
	fp.description( tool_description )
	fp.arguments_description('FILE.v/FOLDER [FILE.v/FOLDER]...')
	fp.limit_free_args_to_at_least(1)
	fp.skip_executable()
	show_help:=fp.bool('help', `h`, false, 'Show this help screen\n')
	if( show_help ){
		println( fp.usage() )
		exit(0)
	}

	mut files := []string{}
	locations := fp.finalize() or { eprintln('Error: ' + err) exit(1) }
	for xloc in locations {
		loc := os.real_path(xloc)
		xfiles := if os.is_dir(loc){ os.walk_ext(loc,'.v') } else { [loc] }
		filtered_files := xfiles.filter(!it.ends_with('_js.v'))
		files << filtered_files
	}

	for file in files {
		analyze_v_file(file)
	}
}
*/
fn main()  {}
