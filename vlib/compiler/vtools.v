module compiler

import os

pub fn launch_tool(tname string){
	vexe := vexe_path()
	vroot := os.dir(vexe)
	mut oargs := os.args
	oargs[0] = '"$vexe"' // make it more explicit
	tool_exe := os.realpath('$vroot/tools/$tname')
	tool_source := os.realpath('$vroot/tools/${tname}.v')
	//////////////////////////////////////////////////////
	tool_args := oargs.join(' ')
	tool_command := '"$tool_exe" $tool_args'
	//println('Launching: "$tool_command" ...')

	mut tool_should_be_recompiled := false
	if !os.exists( tool_exe ) {
		// fresh checkout
		tool_should_be_recompiled = true
	}else{
		if os.file_last_mod_unix( tool_exe ) <= os.file_last_mod_unix( vexe ) {
			// v was recompiled, maybe after v up ...
			// rebuild the tool too just in case
			tool_should_be_recompiled = true
		}
		if os.file_last_mod_unix( tool_exe ) <= os.file_last_mod_unix( tool_source ) {
			// the user changed the source code of the tool
			tool_should_be_recompiled = true
		}
	}
	
	if tool_should_be_recompiled {
		compilation_command := '"$vexe" "$tool_source"'
		//println('Compiling $tname with: "$compilation_command"')
		tool_compilation := os.exec(compilation_command) or { panic(err) }
		if tool_compilation.exit_code != 0 {
			panic('V tool "$tool_source" could not be compiled\n' + tool_compilation.output)
		}
	}
	exit( os.system(tool_command) )
}
