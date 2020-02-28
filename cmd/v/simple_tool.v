// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import (
	compiler
	filepath
	os
	v.pref
)

fn launch_tool(is_verbose bool, tname string, cmdname string) {
	vexe := pref.vexe_path()
	vroot := filepath.dir(vexe)
	compiler.set_vroot_folder(vroot)

	mut tname_index := os.args.index(cmdname)
	if tname_index == -1 {
		tname_index = os.args.len
	}
	mut compilation_options := os.args[1..tname_index].clone()
	tool_args := os.args[1..].join(' ')
	tool_exe := path_of_executable(os.realpath('$vroot/cmd/tools/$tname'))
	tool_source := os.realpath('$vroot/cmd/tools/${tname}.v')
	tool_command := '"$tool_exe" $tool_args'
	if is_verbose {
		eprintln('launch_tool vexe        : $vroot')
		eprintln('launch_tool vroot       : $vroot')
		eprintln('launch_tool tool_args   : $tool_args')
		eprintln('launch_tool tool_command: $tool_command')
	}

	mut should_compile := false
	if !os.exists(tool_exe) {
		should_compile = true
	} else {
		if os.file_last_mod_unix(tool_exe) <= os.file_last_mod_unix(vexe) {
			// v was recompiled, maybe after v up ...
			// rebuild the tool too just in case
			should_compile = true

			if tname == 'vself' || tname == 'vup' {
				// The purpose of vself/up is to update and recompile v itself.
				// After the first 'v self' execution, v will be modified, so
				// then a second 'v self' will detect, that v is newer than the
				// vself executable, and try to recompile vself/up again, which
				// will slow down the next v recompilation needlessly.
				should_compile = false
			}
		}
		if os.file_last_mod_unix(tool_exe) <= os.file_last_mod_unix(tool_source) {
			// the user changed the source code of the tool, or git updated it:
			should_compile = true
		}
	}
	if is_verbose {
		eprintln('launch_tool should_compile: $should_compile')
	}

	if should_compile {
		if tname == 'vfmt' {
			compilation_options << ['-d', 'vfmt']
		}
		compilation_args := compilation_options.join(' ')
		compilation_command := '"$vexe" $compilation_args "$tool_source"'
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

	exit(os.system(tool_command))
}

fn path_of_executable(path string) string {
	$if windows {
		return path + '.exe'
	}
	return path
}
