module main

import os
import os.cmdline
import v.pref
import v.util.recompilation

const is_debug = os.args.contains('-debug')

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
	result := os.execute_or_panic(cmd)
	if result.exit_code != 0 {
		eprintln('cannot compile to `$vroot`: \n$result.output')
		exit(1)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
}

fn list_folder(bmessage string, message string) {
	if !is_debug {
		return
	}
	if bmessage != '' {
		println(bmessage)
	}
	if os.user_os() == 'windows' {
		os.system('dir v*.exe')
	} else {
		os.system('ls -lartd v*')
	}
	println(message)
}

fn backup_old_version_and_rename_newer() ?bool {
	mut errors := []string{}
	short_v_file := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	short_v2_file := if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' }
	short_bak_file := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	v_file := os.real_path(short_v_file)
	v2_file := os.real_path(short_v2_file)
	bak_file := os.real_path(short_bak_file)

	list_folder('before:', 'removing $bak_file ...')
	os.rm(bak_file) or { errors << 'failed removing $bak_file: $err' }

	list_folder('', 'moving $v_file to $bak_file ...')
	os.mv(v_file, bak_file) or { errors << err }

	list_folder('', 'removing $v_file ...')
	os.rm(v_file) or { }

	list_folder('', 'moving $v2_file to $v_file ...')
	os.mv_by_cp(v2_file, v_file) or { panic(err) }

	list_folder('after:', '')

	if errors.len > 0 {
		eprintln('backup errors:\n  >>  ' + errors.join('\n  >>  '))
	}
	return true
}
