module compiler

import os

pub fn launch_tool(tname string){
	vexe := vexe_path()
	vroot := os.dir(vexe)
	mut oargs := os.args
	oargs[0] = vexe // make it more explicit
	tool_exe := os.realpath('$vroot/tools/$tname')
	tool_source := os.realpath('$vroot/tools/${tname}.v')
	//////////////////////////////////////////////////////
	tool_args := oargs.join(' ')
	tool_command := '$tool_exe $tool_args'
	//println('Launching: "$tool_command" ...')
	if !os.file_exists( tool_exe ) {
		compilation_command := '$vexe -prod $tool_source'
		//println('Compiling $tname with: "$compilation_command"')
		tool_compilation := os.exec(compilation_command) or { panic(err) }
		if tool_compilation.exit_code != 0 {
			panic('V tool "$tool_source" could not be compiled.')
		}
	}
	exit( os.system(tool_command) )
}
