module main

import os
import os.cmdline
import v.pref
import v.util.recompilation

const is_debug = os.args.contains('-debug')

fn main() {
	// make testing `v up` easier, by providing a way to force `v self` to fail,
	// to test the fallback logic:
	if os.getenv('VSELF_SHOULD_FAIL') != '' {
		eprintln('v self failed')
		exit(1)
	}
	// support a renamed `v` executable too:
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	vexe_name := os.file_name(vexe)
	short_v_name := vexe_name.all_before('.')
	//
	recompilation.must_be_enabled(vroot, 'Please install V from source, to use `$vexe_name self` .')
	os.chdir(vroot)!
	os.setenv('VCOLORS', 'always', true)
	args := os.args[1..].filter(it != 'self')
	jargs := args.join(' ')
	obinary := cmdline.option(args, '-o', '')
	sargs := if obinary != '' { jargs } else { '$jargs -o v2' }
	cmd := '${os.quoted_path(vexe)} $sargs ${os.quoted_path('cmd/v')}'
	options := if args.len > 0 { '($sargs)' } else { '' }
	println('V self compiling ${options}...')
	compile(vroot, cmd)
	if obinary != '' {
		// When -o was given, there is no need to backup/rename the original.
		// The user just wants an independent copy of v, and so we are done.
		return
	}
	backup_old_version_and_rename_newer(short_v_name) or { panic(err.msg()) }
	println('V built successfully as executable "$vexe_name".')
}

fn compile(vroot string, cmd string) {
	result := os.execute_or_exit(cmd)
	if result.exit_code != 0 {
		eprintln('cannot compile to `$vroot`: \n$result.output')
		exit(1)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
}

fn list_folder(short_v_name string, bmessage string, message string) {
	if !is_debug {
		return
	}
	if bmessage != '' {
		println(bmessage)
	}
	if os.user_os() == 'windows' {
		os.system('dir $short_v_name*.exe')
	} else {
		os.system('ls -lartd $short_v_name*')
	}
	println(message)
}

fn backup_old_version_and_rename_newer(short_v_name string) !bool {
	mut errors := []string{}
	short_v_file := if os.user_os() == 'windows' { '${short_v_name}.exe' } else { '$short_v_name' }
	short_v2_file := if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' }
	short_bak_file := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	v_file := os.real_path(short_v_file)
	v2_file := os.real_path(short_v2_file)
	bak_file := os.real_path(short_bak_file)

	list_folder(short_v_name, 'before:', 'removing $bak_file ...')
	if os.exists(bak_file) {
		os.rm(bak_file) or { errors << 'failed removing $bak_file: $err.msg()' }
	}

	list_folder(short_v_name, '', 'moving $v_file to $bak_file ...')
	os.mv(v_file, bak_file) or { errors << err.msg() }

	list_folder(short_v_name, '', 'removing $v_file ...')
	os.rm(v_file) or {}

	list_folder(short_v_name, '', 'moving $v2_file to $v_file ...')
	os.mv_by_cp(v2_file, v_file) or { panic(err.msg()) }

	list_folder(short_v_name, 'after:', '')

	if errors.len > 0 {
		eprintln('backup errors:\n  >>  ' + errors.join('\n  >>  '))
	}
	return true
}
