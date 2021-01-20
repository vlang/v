module main

import os
import os.cmdline
import v.pref
import v.util.recompilation

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	recompilation.must_be_enabled(vroot, 'Please install V from source, to use `v self` .')
	os.chdir(vroot)
	os.setenv('VCOLORS', 'always', true)
	self_idx := os.args.index('self')
	args := os.args[1..self_idx]
	jargs := args.join(' ')
	obinary := cmdline.option(args, '-o', '')
	sargs := if obinary != '' { jargs } else { '$jargs -o v2' }
	cmd := '$vexe $sargs cmd/v'
	options := if args.len > 0 { '($sargs)' } else { '' }
	println('V self compiling ${options}...')
	compile(vroot, cmd)
	if obinary != '' {
		// When -o was given, there is no need to backup/rename the original.
		// The user just wants an independent copy of v, and so we are done.
		return
	}
	backup_old_version_and_rename_newer() or { panic(err) }
	println('V built successfully!')
}

fn compile(vroot string, cmd string) {
	result := os.exec(cmd) or { panic(err) }
	if result.exit_code != 0 {
		eprintln('cannot compile to `$vroot`: \n$result.output')
		exit(1)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
}

fn backup_old_version_and_rename_newer() ? {
	v_file := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	v2_file := if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' }
	bak_file := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	if os.exists(bak_file) {
		os.rm(bak_file) ?
	}
	os.mv(v_file, bak_file) ?
	os.mv(v2_file, v_file) ?
}
