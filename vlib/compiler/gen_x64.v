module compiler

import filepath

//import compiler.x64

pub fn (v mut V) compile_x64() {
	$if !linux {
		println('v -x64 can only generate Linux binaries for now')
		println('You are not on a Linux system, so you will not ' +
			'be able to run the resulting executable')
	}	
	
	v.files << v.v_files_from_dir(filepath.join(v.pref.vlib_path, 'builtin', 'bare'))
	v.files << v.dir
	v.x64.generate_elf_header()
	for f in v.files {
		v.parse(f, .decl)
	}
	for f in v.files {
		v.parse(f, .main)
	}
	v.x64.generate_elf_footer()
	
}	
