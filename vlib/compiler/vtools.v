module compiler

import (
	os
	filepath
)

pub fn launch_tool(tname string) {
  is_verbose := '-verbose' in os.args || '--verbose' in os.args
	vexe := vexe_path()
	vroot := filepath.dir(vexe)
	set_vroot_folder( vroot ) // needed by tools to find back v
	tool_args := os.args[1..].join(' ')
	tool_exe := os.realpath('$vroot/tools/$tname')
	tool_source := os.realpath('$vroot/tools/${tname}.v')
	tool_command := '"$tool_exe" $tool_args'
	if is_verbose { 
		eprintln('launch_tool vexe        : $vroot')
		eprintln('launch_tool vroot       : $vroot')
		eprintln('launch_tool tool_args   : $tool_args')
		eprintln('launch_tool tool_command: $tool_command')
	}	
	mut tool_should_be_recompiled := false
	if !os.exists(tool_exe) {
		// fresh checkout
		tool_should_be_recompiled = true
	} else {
		if os.file_last_mod_unix(tool_exe) <= os.file_last_mod_unix(vexe) {
			// v was recompiled, maybe after v up ...
			// rebuild the tool too just in case
			tool_should_be_recompiled = true
		}
		if os.file_last_mod_unix(tool_exe) <= os.file_last_mod_unix(tool_source) {
			// the user changed the source code of the tool
			tool_should_be_recompiled = true
		}
	}
	
	if is_verbose {
		eprintln('launch_tool tool_should_be_recompiled: $tool_should_be_recompiled')
	}
	
	if tool_should_be_recompiled {
		mut compilation_options := ''
		if tname == 'vfmt' {  compilation_options = '-d vfmt' }
		compilation_command := '"$vexe" $compilation_options "$tool_source"'
		if is_verbose {
			eprintln('Compiling $tname with: "$compilation_command"') 
		}
		tool_compilation := os.exec(compilation_command) or { panic(err) }
		if tool_compilation.exit_code != 0 {
			panic('V tool "$tool_source" could not be compiled\n' + tool_compilation.output)
		}
	}
	if is_verbose {
		eprintln('launch_tool running tool command: $tool_command ...')
	}
	
	exit( os.system(tool_command) )
}
