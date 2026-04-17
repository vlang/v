module main

import os
import os.cmdline
import v.util.recompilation

const args_ = arguments()
const is_debug = args_.contains('-debug')

// support a renamed `v` executable too:
const vexe = os.getenv_opt('VEXE') or { @VEXE }

const vroot = os.dir(vexe)
const vself_flags_with_values = ['-o', '-os', '-cc', '-gc', '-cf', '-cflags', '-d', '-define']

fn main() {
	// make testing `v up` easier, by providing a way to force `v self` to fail,
	// to test the fallback logic:
	if os.getenv('VSELF_SHOULD_FAIL') != '' {
		eprintln('v self failed')
		exit(1)
	}
	vexe_name := os.file_name(vexe)
	short_v_name := vexe_name.all_before('.')

	recompilation.must_be_enabled(vroot,
		'Please install V from source, to use `${vexe_name} self` .')
	os.chdir(vroot)!
	os.setenv('VCOLORS', 'always', true)
	repeat_count, mut args := extract_repeat_count(args_[1..].filter(it != 'self'))
	if args.len == 0 || ('-cc' !in args && '-prod' !in args && '-parallel-cc' !in args) {
		// compiling by default, i.e. `v self`:
		uos := os.user_os()
		uname := os.uname()
		if uos == 'macos' && uname.machine == 'arm64' {
			// Apple silicon, like m1, m2 etc
			// Use tcc by default for V, since tinycc is much faster and also
			// it already supports compiling many programs like V itself, that do not depend on inlined objective-C code
			args << '-cc tcc'
		} else if uos == 'linux' && uname.machine in ['arm64', 'aarch64'] {
			// Bundled TCC can hang while bootstrapping V on Linux ARM64, so
			// prefer the system compiler for self-builds there.
			args << ['-cc', os.getenv_opt('CC') or { 'cc' }]
		}
	}
	if !has_gc_arg(args) {
		args << ['-gc', 'none']
	}
	jargs := args.join(' ')
	obinary := cmdline.option(args, '-o', '')
	sargs := if obinary != '' { jargs } else { '${jargs} -o v2' }
	options := if args.len > 0 { '(${sargs})' } else { '' }
	final_binary := if obinary != '' { obinary } else { 'v2' }
	pgo_cc_kind := pgo_compiler_kind(args)
	for run_idx in 0 .. repeat_count {
		run_label := if repeat_count > 1 { ' [${run_idx + 1}/${repeat_count}]' } else { '' }
		println('V self compiling${run_label} ${options}...')
		cmd := '${os.quoted_path(vexe)} ${sargs} ${os.quoted_path('cmd/v')}'
		mut used_pgo := false
		if pgo_cc_kind != '' {
			used_pgo = compile_with_pgo(vroot, vexe, args, final_binary, pgo_cc_kind)
			if !used_pgo {
				eprintln('PGO self-build failed; falling back to a regular self-build.')
			}
		}
		if !used_pgo {
			if !try_compile(cmd) {
				bootstrap_self_build(vroot, clone_args(args), final_binary) or {
					eprintln('cannot compile to `${vroot}`: \n${err.msg()}')
					exit(1)
				}
			}
		}
		if obinary == '' {
			backup_old_version_and_rename_newer(short_v_name) or { panic(err.msg()) }
		}
	}
	if obinary != '' {
		return
	}
	println('V built successfully as executable "${vexe_name}".')
}

fn repeat_count_arg(arg string) int {
	if arg.len < 2 || arg[0] != `x` {
		return 0
	}
	for ch in arg[1..].bytes() {
		if !ch.is_digit() {
			return 0
		}
	}
	count := arg[1..].int()
	return if count > 0 { count } else { 0 }
}

fn extract_repeat_count(args []string) (int, []string) {
	mut repeat_count := 1
	mut filtered := []string{cap: args.len}
	mut should_skip_repeat_check := false
	for arg in args {
		if should_skip_repeat_check {
			filtered << arg
			should_skip_repeat_check = false
			continue
		}
		if arg in vself_flags_with_values {
			filtered << arg
			should_skip_repeat_check = true
			continue
		}
		if repeat_count == 1 {
			count := repeat_count_arg(arg)
			if count > 0 {
				repeat_count = count
				continue
			}
		}
		filtered << arg
	}
	return repeat_count, filtered
}

fn has_gc_arg(args []string) bool {
	for arg in args {
		if arg == '-gc' {
			return true
		}
		if arg.starts_with('-gc=') {
			return true
		}
	}
	return false
}

fn has_profile_cflag(args []string) bool {
	mut skip_next := false
	for i, arg in args {
		if skip_next {
			skip_next = false
			continue
		}
		if arg in ['-cflags', '-cf'] {
			if i + 1 < args.len {
				next_arg := args[i + 1]
				if next_arg.contains('-fprofile') {
					return true
				}
				skip_next = true
			}
			continue
		}
		if (arg.starts_with('-cflags=') || arg.starts_with('-cf=')) && arg.contains('-fprofile') {
			return true
		}
	}
	return false
}

fn pgo_compiler_kind(args []string) string {
	if '-prod' !in args || '-no-prod-options' in args {
		return ''
	}
	if os.user_os() == 'windows' {
		return ''
	}
	if has_profile_cflag(args) {
		return ''
	}
	mut ccompiler := cmdline.option(args, '-cc', '')
	if ccompiler == '' {
		ccompiler = os.getenv_opt('CC') or { 'cc' }
	}
	cc_file_name := os.file_name(ccompiler)
	if cc_file_name.contains('clang') || cc_file_name.contains('gcc')
		|| cc_file_name.contains('g++') || ccompiler == 'cc' {
		cc_ver := os.execute('${os.quoted_path(ccompiler)} --version').output
		if cc_ver.contains('clang') {
			_ := find_llvm_profdata() or { return '' }
			return 'clang'
		}
		if cc_ver.contains('Free Software Foundation') || cc_ver.contains('GCC') {
			return 'gcc'
		}
	}
	if cc_file_name.contains('clang') {
		_ := find_llvm_profdata() or { return '' }
		return 'clang'
	}
	if cc_file_name.contains('gcc') || cc_file_name.contains('g++') {
		return 'gcc'
	}
	return ''
}

fn find_llvm_profdata() !string {
	if profdata := os.find_abs_path_of_executable('llvm-profdata') {
		return profdata
	}
	$if macos {
		xcrun_result := os.execute('xcrun --find llvm-profdata')
		if xcrun_result.exit_code == 0 {
			xcrun_path := xcrun_result.output.trim_space()
			if xcrun_path != '' && os.exists(xcrun_path) {
				return xcrun_path
			}
		}
	}
	return error('can not find llvm-profdata in PATH')
}

fn with_output_arg(args []string, output string) []string {
	mut res := []string{cap: args.len + 2}
	mut skip_next := false
	for i, arg in args {
		if skip_next {
			skip_next = false
			continue
		}
		if arg == '-o' {
			if i + 1 < args.len {
				skip_next = true
			}
			continue
		}
		if arg.starts_with('-o=') {
			continue
		}
		res << arg
	}
	res << ['-o', output]
	return res
}

fn clone_args(args []string) []string {
	mut cloned := []string{cap: args.len}
	for arg in args {
		cloned << arg.clone()
	}
	return cloned
}

fn compose_v_cmd(vexe string, args []string, source string) string {
	mut parts := []string{cap: args.len + 2}
	parts << os.quoted_path(vexe)
	for arg in args {
		parts << os.quoted_path(arg)
	}
	parts << os.quoted_path(source)
	return parts.join(' ')
}

fn run_cmd(cmd string) ! {
	result := os.execute(cmd)
	if result.exit_code != 0 {
		return error(result.output)
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
}

fn try_compile(cmd string) bool {
	result := os.execute(cmd)
	if result.exit_code != 0 {
		return false
	}
	if result.output.len > 0 {
		println(result.output.trim_space())
	}
	return true
}

fn compile_with_pgo(vroot string, vexe string, args []string, out_binary string, cc_kind string) bool {
	pgo_workspace := os.join_path(vroot, '.vself_pgo')
	os.rmdir_all(pgo_workspace) or {}
	os.mkdir_all(pgo_workspace) or {
		eprintln('PGO disabled: can not create ${pgo_workspace}: ${err.msg()}')
		return false
	}
	defer {
		os.rmdir_all(pgo_workspace) or {}
	}
	profile_dir := os.join_path(pgo_workspace, 'profile')
	os.mkdir_all(profile_dir) or {
		eprintln('PGO disabled: can not create ${profile_dir}: ${err.msg()}')
		return false
	}
	pgo_binary := os.join_path(pgo_workspace, 'v_pgo_gen')
	training_output := os.join_path(pgo_workspace, 'cmd_v_training.c')
	mut use_profile_flag := '-fprofile-use=${profile_dir}'
	mut llvm_profdata := ''
	mut profile_data := ''
	if cc_kind == 'clang' {
		llvm_profdata = find_llvm_profdata() or {
			eprintln('PGO disabled: can not find `llvm-profdata`.')
			return false
		}
		profile_data = os.join_path(pgo_workspace, 'code.profdata')
		use_profile_flag = '-fprofile-use=${profile_data}'
	}
	mut generate_args := with_output_arg(args, pgo_binary)
	generate_args << ['-cflags', '-fprofile-generate=${profile_dir}']
	generate_cmd := compose_v_cmd(vexe, generate_args, 'cmd/v')
	run_cmd(generate_cmd) or {
		eprintln('PGO step failed while building the instrumented compiler.')
		eprintln(err.msg())
		return false
	}
	training_cmd := '${os.quoted_path(pgo_binary)} -o ${os.quoted_path(training_output)} ${os.quoted_path('cmd/v')}'
	run_cmd(training_cmd) or {
		eprintln('PGO step failed while generating the profiling data.')
		eprintln(err.msg())
		return false
	}
	if cc_kind == 'clang' {
		merge_cmd := '${os.quoted_path(llvm_profdata)} merge -output=${os.quoted_path(profile_data)} ${os.quoted_path(profile_dir)}'
		run_cmd(merge_cmd) or {
			eprintln('PGO step failed while merging the profiling data.')
			eprintln(err.msg())
			return false
		}
	}
	mut final_args := with_output_arg(args, out_binary)
	final_args << ['-cflags', use_profile_flag]
	if cc_kind == 'gcc' {
		final_args << ['-cflags', '-fprofile-correction']
	}
	final_cmd := compose_v_cmd(vexe, final_args, 'cmd/v')
	run_cmd(final_cmd) or {
		eprintln('PGO step failed while building the final compiler binary.')
		eprintln(err.msg())
		return false
	}
	return true
}

fn bootstrap_self_build(vroot string, args []string, final_binary string) ! {
	bootstrap_prefix := '.vself_bootstrap'
	mut bootstrap_v1 := '${bootstrap_prefix}_v1'
	mut bootstrap_v2 := '${bootstrap_prefix}_v2'
	exe_ext := if os.user_os() == 'windows' { '.exe' } else { '' }
	bootstrap_v1 += exe_ext
	bootstrap_v2 += exe_ext
	os.rm(bootstrap_v1) or {}
	os.rm(bootstrap_v2) or {}
	defer {
		os.rm(bootstrap_v1) or {}
		os.rm(bootstrap_v2) or {}
	}
	vc_source := os.join_path(vroot, 'vc', 'v.c')
	if !os.exists(vc_source) {
		return error('bootstrap fallback failed: `${vc_source}` is missing')
	}
	cc := os.getenv_opt('CC') or {
		if os.user_os() == 'windows' { 'gcc' } else { 'cc' }
	}
	bootstrap_v1_build_cmd := bootstrap_c_cmd(cc, bootstrap_v1, vc_source)
	run_cmd(bootstrap_v1_build_cmd) or {
		return error('bootstrap fallback failed while building v1.\n${err.msg()}')
	}
	mut bootstrap_args := ['-no-parallel']
	bootstrap_args << with_output_arg(args, bootstrap_v2)
	bootstrap_v1_cmd := os.join_path('.', bootstrap_v1)
	bootstrap_v2_cmd := '${os.quoted_path(bootstrap_v1_cmd)} ${bootstrap_args.join(' ')} ${os.quoted_path('cmd/v')}'
	run_cmd(bootstrap_v2_cmd) or {
		return error('bootstrap fallback failed while building v2.\n${err.msg()}')
	}
	final_args := with_output_arg(args, final_binary)
	bootstrap_v2_cmd_path := os.join_path('.', bootstrap_v2)
	final_cmd := '${os.quoted_path(bootstrap_v2_cmd_path)} ${final_args.join(' ')} ${os.quoted_path('cmd/v')}'
	run_cmd(final_cmd) or {
		return error('bootstrap fallback failed while building the final compiler.\n${err.msg()}')
	}
}

fn bootstrap_c_cmd(cc string, out_binary string, vc_source string) string {
	mut parts := []string{cap: 8}
	parts << os.quoted_path(cc)
	if os.user_os() == 'windows' {
		parts << ['-std=c99', '-municode', '-w', '-o', os.quoted_path(out_binary),
			os.quoted_path(vc_source), '-lws2_32']
	} else {
		parts << ['-std=c99', '-w', '-o', os.quoted_path(out_binary),
			os.quoted_path(vc_source), '-lm', '-lpthread']
	}
	return parts.join(' ')
}

fn list_folder(short_v_name string, bmessage string, message string) {
	if !is_debug {
		return
	}
	if bmessage != '' {
		println(bmessage)
	}
	if os.user_os() == 'windows' {
		os.system('dir ${short_v_name}*.exe')
	} else {
		os.system('ls -lartd ${short_v_name}*')
	}
	println(message)
}

fn backup_old_version_and_rename_newer(short_v_name string) !bool {
	mut errors := []string{}
	short_v_file := if os.user_os() == 'windows' { '${short_v_name}.exe' } else { '${short_v_name}' }
	short_v2_file := if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' }
	short_bak_file := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	v_file := os.real_path(short_v_file)
	v2_file := os.real_path(short_v2_file)
	bak_file := os.real_path(short_bak_file)

	list_folder(short_v_name, 'before:', 'removing ${bak_file} ...')
	if os.exists(bak_file) {
		os.rm(bak_file) or { errors << 'failed removing ${bak_file}: ${err.msg()}' }
	}

	list_folder(short_v_name, '', 'moving ${v_file} to ${bak_file} ...')
	os.mv(v_file, bak_file) or { errors << err.msg() }

	list_folder(short_v_name, '', 'removing ${v_file} ...')
	os.rm(v_file) or {}

	list_folder(short_v_name, '', 'moving ${v2_file} to ${v_file} ...')
	os.mv_by_cp(v2_file, v_file) or { panic(err.msg()) }

	list_folder(short_v_name, 'after:', '')

	if errors.len > 0 {
		eprintln('backup errors:\n  >>  ' + errors.join('\n  >>  '))
	}
	return true
}
