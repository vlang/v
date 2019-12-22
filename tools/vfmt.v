// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import (
	os
	compiler
)

fn main() {
	is_w := '-w' in os.args
	is_diff := '-diff' in os.args
	is_verbose := '--verbose' in os.args || '-verbose' in os.args
	
	toolexe := os.executable()
	compiler.set_vroot_folder( os.dir(os.dir(toolexe)) )
	args := compiler.env_vflags_and_os_args()
	
	file := args.last()
	if !os.exists(file) { 
		compiler.verror('"$file" does not exist.') 
	}
	if !file.ends_with('.v') {
		compiler.verror('v fmt can only be used on .v files')
	}
	
	mut v := compiler.new_v_compiler_with_args([file])
	
	if is_verbose {
		eprintln('vfmt toolexe: $toolexe')
		eprintln('vfmt args: ' + os.args.str())
		eprintln('vfmt env_vflags_and_os_args: ' + args.str())
		eprintln('vfmt format_file: $file | v.dir: $v.dir')
	}
	
	v.compile()
	
	formatted_file_path := os.getenv('VFMT_FILE_RESULT')
	//eprintln('Formatted file is: $formatted_file_path .')  

	if is_diff {
		if find_diff := os.exec('diff -v') {
			os.system('diff "$formatted_file_path" "$file" ')
			exit(0)
		}
		eprintln('No working "diff" CLI command found.')
	}
	
	if is_w {  
		os.mv_by_cp( formatted_file_path, file ) or { panic(err) }
		eprintln('Reformatted file in place: $file .')
	}else{
		content := os.read_file( formatted_file_path ) or { panic(err) }
		print( content )  
	}
	
}
