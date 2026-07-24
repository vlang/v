module builder

import os
import v.ast
import v.checker
import v.parser
import v.pkgconfig
import v.pref

const issue74_fixture_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'pkgconfig', 'testdata',
	'static_pkgconfig')

const issue74_source_order_source = 'module main
#flag -Wl,--issue74-order-before
#pkgconfig --static --cflags --libs source-order-74
#flag -Wl,--issue74-order-between
#pkgconfig --static --cflags --libs source-order-74
#flag -Wl,--issue74-order-after

fn main() {}
'

const issue74_repeated_control_epochs_source = 'module main
#flag -Wl,--whole-archive
#pkgconfig --static --cflags --libs source-order-74
#flag -Wl,--no-whole-archive
#flag -Wl,--whole-archive
#pkgconfig --static --cflags --libs source-order-74
#flag -Wl,--no-whole-archive

fn main() {}
'

const issue74_dynamic_compat_source = 'module main
#pkgconfig --cflags --libs mixed-case-dynamic-sentinel-74 mixed-case-static-74

fn main() {}
'

const issue74_repeated_control_dynamic_source = 'module main
#flag -Wl,--whole-archive
#pkgconfig --cflags --libs mixed-case-dynamic-sentinel-74
#flag -Wl,--no-whole-archive
#flag -Wl,--whole-archive
#flag -Wl,--no-whole-archive

fn main() {}
'

const issue74_ldflags_static_source = 'module main
#pkgconfig static-root-74

fn main() {}
'

const issue74_group_alias_source = 'module main
#pkgconfig --static --libs group-alias-74

fn main() {}
'

const issue74_xlinker_operand_source = 'module main
#flag -Xlinker
#flag --issue74-xlinker-operand
#pkgconfig --static --cflags --libs source-order-74

fn main() {}
'

const issue74_xlinker_interrupted_source = 'module main
#flag -Xlinker
#pkgconfig --static --cflags --libs source-order-74
#flag --issue74-xlinker-operand

fn main() {}
'

const issue74_xlinker_end_of_stream_source = 'module main
#pkgconfig --static --cflags --libs source-order-74
#flag -Xlinker

fn main() {}
'

const issue74_wl_rpath_interrupted_source = 'module main
#flag -Wl,-rpath
#pkgconfig --static --cflags --libs source-order-74
#flag -Wl,/issue74/rpath

fn main() {}
'

const issue74_version_script_interrupted_source = 'module main
#flag -Wl,--version-script
#pkgconfig --static --cflags --libs source-order-74
#flag /issue74/exports.map

fn main() {}
'

fn issue74_wl_operand_source(control string, operand string, interrupted bool) string {
	ordered_flags := if interrupted {
		'#flag ${control}\n#pkgconfig --static --cflags --libs source-order-74\n#flag ${operand}'
	} else {
		'#flag ${control}\n#flag ${operand}\n#pkgconfig --static --cflags --libs source-order-74'
	}
	return 'module main\n${ordered_flags}\n\nfn main() {}\n'
}

const issue74_source_order_markers = ['-Wl,--issue74-order-before', '-Wl,--issue74-order-pkg-public',
	'-Wl,--issue74-order-pkg-private', '-Wl,--issue74-order-between',
	'-Wl,--issue74-order-pkg-public', '-Wl,--issue74-order-pkg-private', '-Wl,--issue74-order-after']

const issue74_xlinker_operand_markers = ['--issue74-xlinker-operand',
	'-Wl,--issue74-order-pkg-public', '-Wl,--issue74-order-pkg-private']

const issue74_static_linker_markers = ['-lissue74_root', '-Wl,--start-group', '-lissue74_cycle_a',
	'-lissue74_cycle_b', '-lissue74_cycle_a', '-Wl,--end-group', '-lissue74_root_private',
	'-lissue74_public', '-lissue74_public_private', '-lissue74_private', '-lissue74_private_private']

const issue74_group_alias_markers = ['-Wl,-(', '-lissue74_alias_a', '-lissue74_alias_b',
	'-lissue74_alias_a', '-Wl,-)']

const issue74_windows_fake_ccompiler_source = 'module main

import os

fn main() {
	log_path := os.getenv("ISSUE74_CC_LOG")
	if log_path == "" {
		exit(2)
	}
	mut log := os.open_append(log_path) or { panic(err) }
	defer {
		log.close()
	}
	mut output := ""
	mut take_output := false
	for arg in os.args[1..] {
		log.writeln(arg) or { panic(err) }
		if take_output {
			output = arg
			take_output = false
		} else if arg == "-o" {
			take_output = true
		}
	}
	if output != "" {
		os.mkdir_all(os.dir(output)) or { panic(err) }
		os.write_file(output, "") or { panic(err) }
	}
}
'

struct Issue74DriverEnvironment {
mut:
	has_cflags  bool
	cflags      string
	has_ldflags bool
	ldflags     string
}

fn issue74_clear_driver_environment() Issue74DriverEnvironment {
	mut environment := Issue74DriverEnvironment{}
	if value := os.getenv_opt('CFLAGS') {
		environment.has_cflags = true
		environment.cflags = value
	}
	if value := os.getenv_opt('LDFLAGS') {
		environment.has_ldflags = true
		environment.ldflags = value
	}
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	return environment
}

fn (environment &Issue74DriverEnvironment) restore() {
	if environment.has_cflags {
		os.setenv('CFLAGS', environment.cflags, true)
	} else {
		os.unsetenv('CFLAGS')
	}
	if environment.has_ldflags {
		os.setenv('LDFLAGS', environment.ldflags, true)
	} else {
		os.unsetenv('LDFLAGS')
	}
}

fn issue74_builder_for_source(source string) Builder {
	driver_environment := issue74_clear_driver_environment()
	defer {
		driver_environment.restore()
	}
	old_path := os.getenv_opt('PKG_CONFIG_PATH')
	old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
	os.unsetenv('PKG_CONFIG_PATH')
	os.setenv('PKG_CONFIG_PATH_DEFAULTS', issue74_fixture_dir, true)
	defer {
		if path := old_path {
			os.setenv('PKG_CONFIG_PATH', path, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH')
		}
		if defaults := old_defaults {
			os.setenv('PKG_CONFIG_PATH_DEFAULTS', defaults, true)
		} else {
			os.unsetenv('PKG_CONFIG_PATH_DEFAULTS')
		}
	}
	source_path := os.join_path('/virtual', 'issue74_pkgconfig_order.v')
	mut prefs, _ := pref.parse_args([], ['-cc', 'gcc', source_path])
	prefs.out_name = os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_order.out')
	mut table := ast.new_table()
	mut file := parser.parse_text(source, source_path, mut table, .skip_comments, prefs)
	mut chk := checker.new_checker(table, prefs)
	chk.check(mut file)
	mut builder := new_builder(prefs)
	builder.table = table
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_order.tmp.c')
	builder.setup_ccompiler_options('gcc')
	builder.setup_output_name()
	return builder
}

fn issue74_pkgconfig_builder_for_target(flags []string, target_os pref.OS, compiler_type pref.CompilerType) Builder {
	source_path := os.join_path('/virtual', 'issue74_pkgconfig_target.v')
	mut prefs, _ := pref.parse_args([], ['', source_path])
	prefs.os = target_os
	prefs.ccompiler_type = compiler_type
	mut table := ast.new_table()
	table.parse_pkgconfig_link_flags(flags, 'main', prefs.compile_defines_all) or { panic(err) }
	mut builder := new_builder(prefs)
	builder.table = table
	return builder
}

fn issue74_ordered_pkgconfig_args_for_target(flags []string, target_os pref.OS, compiler_type pref.CompilerType) []string {
	mut builder := issue74_pkgconfig_builder_for_target(flags, target_os, compiler_type)
	_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
	return ordered_args
}

fn issue74_quoted_direct_import_lib_case() ([]string, string) {
	lib_dir := os.join_path(os.vtmp_dir(), 'issue74 search libs')
	direct_lib := os.join_path(os.vtmp_dir(), 'issue74 direct libs', 'Issue74Direct.LiB')
	flags := ['-L"${lib_dir}"', '"${direct_lib}"', 'Issue74Mixed.LiB']
	return flags, direct_lib
}

struct Issue74ToolResult {
	exit_code int
	stdout    string
	stderr    string
}

fn issue74_run_tool(executable string, args []string, work_dir string) Issue74ToolResult {
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_redirect_stdio()
	process.wait()
	result := Issue74ToolResult{
		exit_code: process.code
		stdout:    process.stdout_slurp()
		stderr:    process.stderr_slurp()
	}
	process.close()
	return result
}

fn issue74_is_source_order_marker(arg string) bool {
	return arg.starts_with('-Wl,--issue74-order-')
}

fn test_pkgconfig_linker_segments_keep_source_relative_order_and_repetition() {
	mut builder := issue74_builder_for_source(issue74_source_order_source)
	all_args := builder.all_args(builder.ccoptions)
	markers := all_args.filter(issue74_is_source_order_marker(it))
	assert markers == issue74_source_order_markers, all_args.str()

	generated_source_args := all_args.filter(it.contains(os.file_name(builder.out_name_c)))
	assert generated_source_args.len == 1, all_args.str()
	generated_idx := all_args.index(generated_source_args[0])
	assert all_args.index(issue74_source_order_markers[0]) > generated_idx, all_args.str()
}

fn test_repeated_ordered_flag_control_epochs_are_preserved() {
	mut builder := issue74_builder_for_source(issue74_repeated_control_epochs_source)
	global_controls := builder.table.cflags.filter(it.name == '-Wl'
		&& it.value in [',--whole-archive', ',--no-whole-archive'])
	assert global_controls.len == 2

	all_args := builder.all_args(builder.ccoptions)
	ordered := all_args.filter(it in ['-Wl,--whole-archive', '-Wl,--no-whole-archive']
		|| it.starts_with('-Wl,--issue74-order-pkg-'))
	assert ordered == [
		'-Wl,--whole-archive',
		'-Wl,--issue74-order-pkg-public',
		'-Wl,--issue74-order-pkg-private',
		'-Wl,--no-whole-archive',
		'-Wl,--whole-archive',
		'-Wl,--issue74-order-pkg-public',
		'-Wl,--issue74-order-pkg-private',
		'-Wl,--no-whole-archive',
	], all_args.str()
}

fn test_cross_module_repeated_ordered_flag_epochs_link_and_run() {
	root := os.join_path(os.vtmp_dir(), 'issue74_cross_module_epochs')
	archive_a := os.join_path(root, 'libissue74_epoch_a.a')
	archive_b := os.join_path(root, 'libissue74_epoch_b.a')
	expected_archive_a := if archive_a.contains_any(' \t\r\n') {
		'"${archive_a}"'
	} else {
		archive_a
	}
	expected_archive_b := if archive_b.contains_any(' \t\r\n') {
		'"${archive_b}"'
	} else {
		archive_b
	}
	mut table := ast.new_table()
	modules := ['issue74_epoch_a', 'issue74_epoch_b']
	archives := [archive_a, archive_b]
	for i, mod_name in modules {
		archive := archives[i]
		table.parse_cflag_with_link_segment('-Wl,--whole-archive', mod_name, []) or { panic(err) }
		table.parse_pkgconfig_link_flags([archive], mod_name, []) or { panic(err) }
		table.parse_cflag_with_link_segment('-Wl,--no-whole-archive', mod_name, []) or {
			panic(err)
		}
	}
	assert table.cflags.len == 2
	assert table.cflags.all(it.mod == 'issue74_epoch_a')

	mut prefs, _ := pref.parse_args([], ['', os.join_path(root, 'main.v')])
	prefs.os = .linux
	prefs.ccompiler_type = .gcc
	mut builder := new_builder(prefs)
	builder.table = table
	legacy, ordered, _ := builder.split_ordered_pkgconfig_link_flags(builder.table.cflags)
	assert legacy == []
	assert ordered == [
		'-Wl,--whole-archive',
		expected_archive_a,
		'-Wl,--no-whole-archive',
		'-Wl,--whole-archive',
		expected_archive_b,
		'-Wl,--no-whole-archive',
	]

	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		ar := os.find_abs_path_of_executable('ar') or { return }
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_source := os.join_path(root, 'main.c')
		a_source := os.join_path(root, 'epoch_a.c')
		b_source := os.join_path(root, 'epoch_b.c')
		a_object := os.join_path(root, 'epoch_a.o')
		b_object := os.join_path(root, 'epoch_b.o')
		output := os.join_path(root, 'cross_module_epochs.out')
		os.write_file(main_source,
			'int issue74_epoch_total = 0;\nint main(void) { return issue74_epoch_total == 74 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(a_source,
			'extern int issue74_epoch_total;\n__attribute__((constructor)) static void issue74_epoch_a(void) { issue74_epoch_total += 1; }\n') or {
			panic(err)
		}
		os.write_file(b_source,
			'extern int issue74_epoch_total;\n__attribute__((constructor)) static void issue74_epoch_b(void) { issue74_epoch_total += 73; }\n') or {
			panic(err)
		}
		for source, object in {
			a_source: a_object
			b_source: b_object
		} {
			compiled := issue74_run_tool(gcc, ['-c', source, '-o', object], root)
			assert compiled.exit_code == 0, compiled.stdout + compiled.stderr
		}
		for archive, object in {
			archive_a: a_object
			archive_b: b_object
		} {
			archived := issue74_run_tool(ar, ['rcs', archive, object], root)
			assert archived.exit_code == 0, archived.stdout + archived.stderr
		}
		mut link_args := [main_source]
		link_args << ordered
		link_args << ['-o', output]
		linked := issue74_run_tool(gcc, link_args, root)
		assert linked.exit_code == 0, linked.stdout + linked.stderr
		assert issue74_run_tool(output, [], root).exit_code == 0
	}
}

fn test_split_version_script_before_pkgconfig_links_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_version_script_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		version_script := os.join_path(root, 'exports.map')
		os.write_file(main_source, 'int main(void) { return 0; }\n') or { panic(err) }
		os.write_file(version_script, 'VERS_1 { global: *; };\n') or { panic(err) }

		mut table := ast.new_table()
		table.parse_cflag_with_link_segment('-Wl,--version-script', 'main', []) or { panic(err) }
		table.parse_cflag_with_link_segment(version_script, 'main', []) or { panic(err) }
		table.parse_pkgconfig_link_flags(['-lm'], 'main', []) or { panic(err) }
		mut prefs, _ := pref.parse_args([], ['', main_source])
		prefs.os = .linux
		prefs.ccompiler_type = .gcc
		mut builder := new_builder(prefs)
		builder.table = table
		legacy, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags(builder.table.cflags)
		assert legacy == []
		assert ordered_args == ['-Wl,--version-script', version_script, '-lm']

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_link := issue74_run_tool(gcc, direct_args, root)
		assert direct_link.exit_code == 0, direct_link.stdout + direct_link.stderr
		assert issue74_run_tool(direct_output, [], root).exit_code == 0

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
	}
}

fn test_static_pkgconfig_split_version_script_with_spaced_operand_links_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_version_script_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		version_script := os.join_path(root, 'exports map.map')
		pc_path := os.join_path(root, 'split-version-script.pc')
		os.write_file(main_source, 'int main(void) { return 0; }\n') or { panic(err) }
		os.write_file(version_script, 'VERS_1 { global: *; };\n') or { panic(err) }
		os.write_file(pc_path,
			'Name: split-version-script\nDescription: disposable split version-script fixture\nVersion: 1.0.0\nLibs: -Wl,--version-script "${version_script}"\n') or {
			panic(err)
		}

		old_path := os.getenv_opt('PKG_CONFIG_PATH')
		old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
		os.setenv('PKG_CONFIG_PATH', root, true)
		os.setenv('PKG_CONFIG_PATH_DEFAULTS', '', true)
		defer {
			if path := old_path {
				os.setenv('PKG_CONFIG_PATH', path, true)
			} else {
				os.unsetenv('PKG_CONFIG_PATH')
			}
			if defaults := old_defaults {
				os.setenv('PKG_CONFIG_PATH_DEFAULTS', defaults, true)
			} else {
				os.unsetenv('PKG_CONFIG_PATH_DEFAULTS')
			}
		}
		mut command := pkgconfig.main(['--static', '--libs', 'split-version-script']) or {
			panic(err)
		}
		result := command.run_result() or { panic(err) }
		assert result.link_flags == ['-Wl,--version-script', version_script]

		mut builder := issue74_pkgconfig_builder_for_target(result.link_flags, .linux, .gcc)
		_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
		assert ordered_args == ['-Wl,--version-script', '"${version_script}"']

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_command := '${os.quoted_path(gcc)} ${direct_args.map(shell_safe_cc_arg(it)).join(' ')}'
		direct_link := os.execute(direct_command)
		assert direct_link.exit_code == 0, direct_link.output
		assert issue74_run_tool(direct_output, [], root).exit_code == 0

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
	}
}

fn test_repeated_ordered_flag_controls_keep_dynamic_legacy_deduplication() {
	mut builder := issue74_builder_for_source(issue74_repeated_control_dynamic_source)
	assert !builder.table.link_flag_segments.any(it.is_pkgconfig)
	all_args := builder.all_args(builder.ccoptions)
	assert all_args.count(it == '-Wl,--whole-archive') == 1, all_args.str()
	assert all_args.count(it == '-Wl,--no-whole-archive') == 1, all_args.str()
	assert all_args.count(it == '-Wl,--issue74-dynamic-sentinel') == 1, all_args.str()
}

fn test_static_pkgconfig_structured_archives_link_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		ar := os.find_abs_path_of_executable('ar') or { return }
		root := os.join_path('/tmp', 'issue74_structured_link_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		a_source := os.join_path(root, 'a.c')
		b_source := os.join_path(root, 'b.c')
		a_object := os.join_path(root, 'a.o')
		b_object := os.join_path(root, 'b.o')
		lib_dir := os.join_path(root, 'lib')
		os.mkdir_all(lib_dir) or { panic(err) }
		lib_a := os.join_path(lib_dir, 'libissue74_a.a')
		lib_b := os.join_path(lib_dir, 'libissue74_b.a')
		os.write_file(main_source,
			'int issue74_a(void);\nint main(void) { return issue74_a() == 74 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(a_source,
			'int issue74_b(void);\nint issue74_a(void) { return issue74_b() + 1; }\n') or {
			panic(err)
		}
		os.write_file(b_source, 'int issue74_b(void) { return 73; }\n') or { panic(err) }

		for source, object in {
			a_source: a_object
			b_source: b_object
		} {
			compiled := issue74_run_tool(gcc, ['-c', source, '-o', object], root)
			assert compiled.exit_code == 0, compiled.stdout + compiled.stderr
		}
		for archive, object in {
			lib_a: a_object
			lib_b: b_object
		} {
			archived := issue74_run_tool(ar, ['rcs', archive, object], root)
			assert archived.exit_code == 0, archived.stdout + archived.stderr
		}

		pc_path := os.join_path(root, 'structured-link-real.pc')
		os.write_file(pc_path,
			'libdir=${lib_dir}\n\nName: structured-link-real\nDescription: disposable structured static link fixture\nVersion: 1.0.0\nLibs.private: -L\${libdir} \${libdir}/libissue74_b.a \${libdir}/libissue74_a.a \${libdir}/libissue74_b.a\n') or {
			panic(err)
		}
		old_path := os.getenv_opt('PKG_CONFIG_PATH')
		old_defaults := os.getenv_opt('PKG_CONFIG_PATH_DEFAULTS')
		os.setenv('PKG_CONFIG_PATH', root, true)
		os.setenv('PKG_CONFIG_PATH_DEFAULTS', '', true)
		defer {
			if path := old_path {
				os.setenv('PKG_CONFIG_PATH', path, true)
			} else {
				os.unsetenv('PKG_CONFIG_PATH')
			}
			if defaults := old_defaults {
				os.setenv('PKG_CONFIG_PATH_DEFAULTS', defaults, true)
			} else {
				os.unsetenv('PKG_CONFIG_PATH_DEFAULTS')
			}
		}
		mut command := pkgconfig.main(['--static', '--libs', 'structured-link-real']) or {
			panic(err)
		}
		result := command.run_result() or { panic(err) }
		assert result.link_flags == ['-L${lib_dir}', lib_b, lib_a, lib_b]

		mut builder := issue74_pkgconfig_builder_for_target(result.link_flags, .linux, .gcc)
		_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
		assert ordered_args == ['-L"${os.real_path(lib_dir)}"', lib_b, lib_a, lib_b]

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_link := issue74_run_tool(gcc, direct_args, root)
		assert direct_link.exit_code == 0, direct_link.stdout + direct_link.stderr
		assert issue74_run_tool(direct_output, [], root).exit_code == 0

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
	}
}

fn test_static_pkgconfig_quoted_search_path_routing_is_bounded() {
	search_path := os.join_path(os.vtmp_dir(), 'issue74 path - with spaces')
	direct_archive := os.join_path(os.vtmp_dir(), 'issue74 direct - archives', 'lib x.a')
	direct_msvc_lib := os.join_path(os.vtmp_dir(), 'issue74 direct - archives', 'issue74 x.LIB')
	flags := ['-L"${search_path}"', '"${direct_archive}"', '"${direct_msvc_lib}"']
	expected := ['-L"${os.real_path(search_path)}"', '"${direct_archive}"', '"${direct_msvc_lib}"']

	for compiler_type in [pref.CompilerType.gcc, .clang, .tinyc, .cplusplus] {
		args := issue74_ordered_pkgconfig_args_for_target(flags, .linux, compiler_type)
		assert args == expected, '${compiler_type}: ${args}'
	}
	windows_clang_args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, .clang)
	assert windows_clang_args == expected, windows_clang_args.str()

	for compiler_type in [pref.CompilerType.gcc, .mingw] {
		args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, compiler_type)
		assert args == expected, '${compiler_type}: ${args}'
	}

	msvc_args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, .msvc)
	assert msvc_args == expected, msvc_args.str()

	raw_expected := ['"${direct_archive}"']
	for compiler_type in [pref.CompilerType.gcc, .clang, .tinyc, .cplusplus] {
		args := issue74_ordered_pkgconfig_args_for_target([direct_archive], .linux, compiler_type)
		assert args == raw_expected, '${compiler_type}: ${args}'
	}
	for compiler_type in [pref.CompilerType.gcc, .mingw, .clang, .msvc] {
		args := issue74_ordered_pkgconfig_args_for_target([direct_archive], .windows, compiler_type)
		assert args == raw_expected, '${compiler_type}: ${args}'
	}
}

fn test_static_pkgconfig_quoted_search_path_links_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		ar := os.find_abs_path_of_executable('ar') or { return }
		root := os.join_path('/tmp', 'issue74_quoted_search_link_${os.getpid()}')
		lib_dir := os.join_path(root, 'path - with spaces')
		os.rmdir_all(root) or {}
		os.mkdir_all(lib_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		library_source := os.join_path(root, 'library.c')
		library_object := os.join_path(root, 'library.o')
		library_path := os.join_path(lib_dir, 'libissue74_quoted_search.a')
		os.write_file(main_source,
			'int issue74_quoted_search(void);\nint main(void) { return issue74_quoted_search() == 74 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(library_source, 'int issue74_quoted_search(void) { return 74; }\n') or {
			panic(err)
		}
		compiled := issue74_run_tool(gcc, ['-c', library_source, '-o', library_object], root)
		assert compiled.exit_code == 0, compiled.stdout + compiled.stderr
		archived := issue74_run_tool(ar, ['rcs', library_path, library_object], root)
		assert archived.exit_code == 0, archived.stdout + archived.stderr

		mut builder := issue74_pkgconfig_builder_for_target(['-L"${lib_dir}"',
			'-lissue74_quoted_search'], .linux, .gcc)
		_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
		assert ordered_args == ['-L"${os.real_path(lib_dir)}"', '-lissue74_quoted_search']

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_command := '${os.quoted_path(gcc)} ${direct_args.map(shell_safe_cc_arg(it)).join(' ')}'
		direct_link := os.execute(direct_command)
		assert direct_link.exit_code == 0, direct_link.output
		assert issue74_run_tool(direct_output, [], root).exit_code == 0

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
	}
}

fn test_static_pkgconfig_raw_spaced_colon_archive_links_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		ar := os.find_abs_path_of_executable('ar') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_raw_spaced_archive_${os.getpid()}')
		lib_dir := os.join_path(root, 'direct:archives with spaces')
		os.rmdir_all(root) or {}
		os.mkdir_all(lib_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		library_source := os.join_path(root, 'library.c')
		library_object := os.join_path(root, 'library.o')
		library_path := os.join_path(lib_dir, 'libissue74_raw_spaced.a')
		os.write_file(main_source,
			'int issue74_raw_spaced(void);\nint main(void) { return issue74_raw_spaced() == 74 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(library_source, 'int issue74_raw_spaced(void) { return 74; }\n') or {
			panic(err)
		}
		compiled := issue74_run_tool(gcc, ['-c', library_source, '-o', library_object], root)
		assert compiled.exit_code == 0, compiled.stdout + compiled.stderr
		archived := issue74_run_tool(ar, ['rcs', library_path, library_object], root)
		assert archived.exit_code == 0, archived.stdout + archived.stderr

		mut builder := issue74_pkgconfig_builder_for_target([library_path], .linux, .gcc)
		_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
		assert ordered_args == ['"${library_path}"']

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_command := '${os.quoted_path(gcc)} ${direct_args.map(shell_safe_cc_arg(it)).join(' ')}'
		direct_link := os.execute(direct_command)
		assert direct_link.exit_code == 0, direct_link.output
		assert issue74_run_tool(direct_output, [], root).exit_code == 0

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
	}
}

fn test_static_pkgconfig_spaced_wl_rpath_links_direct_and_response_file() {
	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		readelf := os.find_abs_path_of_executable('readelf') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_spaced_wl_rpath_${os.getpid()}')
		lib_dir := os.join_path(root, 'runtime libs with spaces')
		os.rmdir_all(root) or {}
		os.mkdir_all(lib_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}

		main_source := os.join_path(root, 'main.c')
		library_source := os.join_path(root, 'library.c')
		library_path := os.join_path(lib_dir, 'libissue74_spaced_rpath.so')
		os.write_file(main_source,
			'int issue74_spaced_rpath(void);\nint main(void) { return issue74_spaced_rpath() == 74 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(library_source, 'int issue74_spaced_rpath(void) { return 74; }\n') or {
			panic(err)
		}
		shared_link := issue74_run_tool(gcc,
			['-fPIC', '-shared', library_source, '-o', library_path], root)
		assert shared_link.exit_code == 0, shared_link.stdout + shared_link.stderr

		mut builder := issue74_pkgconfig_builder_for_target(['-L${lib_dir}', '-lissue74_spaced_rpath',
			'-Wl,-rpath,${lib_dir}'], .linux, .gcc)
		_, ordered_args, _ := builder.split_ordered_pkgconfig_link_flags([])
		assert ordered_args == ['-L"${os.real_path(lib_dir)}"', '-lissue74_spaced_rpath',
			'"-Wl,-rpath,${lib_dir}"']

		direct_output := os.join_path(root, 'direct.out')
		mut direct_args := [main_source]
		direct_args << ordered_args
		direct_args << ['-o', direct_output]
		direct_command := '${os.quoted_path(gcc)} ${direct_args.map(shell_safe_cc_arg(it)).join(' ')}'
		direct_link := os.execute(direct_command)
		assert direct_link.exit_code == 0, direct_link.output
		assert issue74_run_tool(direct_output, [], root).exit_code == 0
		direct_dynamic := issue74_run_tool(readelf, ['-d', direct_output], root)
		assert direct_dynamic.exit_code == 0, direct_dynamic.stdout + direct_dynamic.stderr
		direct_runpaths := direct_dynamic.stdout.split_into_lines().filter(it.contains('(RUNPATH)'))
		assert direct_runpaths.len == 1, direct_dynamic.stdout
		assert direct_runpaths[0].all_after('Library runpath: [').all_before(']') == lib_dir

		rsp_output := os.join_path(root, 'rsp.out')
		mut rsp_args := [main_source]
		rsp_args << ordered_args
		rsp_args << ['-o', rsp_output]
		rsp_args = rsp_args.map(builder.rsp_safe_arg(it))
		assert builder.should_use_rsp(rsp_args)
		rsp_path := os.join_path(root, 'link.rsp')
		os.write_file(rsp_path, rsp_args.join(' ').replace('\\', '\\\\')) or { panic(err) }
		rsp_link := issue74_run_tool(gcc, ['@${rsp_path}'], root)
		assert rsp_link.exit_code == 0, rsp_link.stdout + rsp_link.stderr
		assert issue74_run_tool(rsp_output, [], root).exit_code == 0
		rsp_dynamic := issue74_run_tool(readelf, ['-d', rsp_output], root)
		assert rsp_dynamic.exit_code == 0, rsp_dynamic.stdout + rsp_dynamic.stderr
		rsp_runpaths := rsp_dynamic.stdout.split_into_lines().filter(it.contains('(RUNPATH)'))
		assert rsp_runpaths.len == 1, rsp_dynamic.stdout
		assert rsp_runpaths[0].all_after('Library runpath: [').all_before(']') == lib_dir
	}
}

fn test_windows_gnu_static_pkgconfig_converts_only_bare_import_libs() {
	lib_dir := os.join_path(os.vtmp_dir(), 'issue74_windows_gnu_libs')
	path_lib := os.join_path(lib_dir, 'issue74_path.lib')
	expected_path_lib := if path_lib.contains_any(' \t\r\n') { '"${path_lib}"' } else { path_lib }
	flags := ['-L${lib_dir}', 'issue74_direct.lib', '-lissue74_anchor', 'issue74_repeat.lib',
		path_lib, 'issue74_upper.LIB', 'issue74_direct.lib', 'issue74_repeat.lib']
	expected_tail := ['-lissue74_direct', '-lissue74_anchor', '-lissue74_repeat', expected_path_lib,
		'-lissue74_upper', '-lissue74_direct', '-lissue74_repeat']
	for compiler_type in [pref.CompilerType.gcc, .mingw] {
		args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, compiler_type)
		assert args.len == expected_tail.len + 1, '${compiler_type}: ${args}'
		assert args[0].starts_with('-L'), '${compiler_type}: ${args}'
		assert args[0].contains(os.real_path(lib_dir)), '${compiler_type}: ${args}'
		assert args[1..] == expected_tail, '${compiler_type}: ${args}'
	}

	posix_args := issue74_ordered_pkgconfig_args_for_target(flags, .linux, .gcc)
	assert !posix_args.any(it in ['-lissue74_direct', '-lissue74_repeat', '-lissue74_upper']), posix_args.str()
	assert posix_args.join(' ').contains('issue74_direct.lib'), posix_args.str()
	for compiler_type in [pref.CompilerType.clang, .msvc] {
		args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, compiler_type)
		assert args == posix_args, '${compiler_type}: ${args}'
	}
}

fn test_windows_gnu_static_pkgconfig_quotes_spaced_wl_before_import_conversion() {
	rpath := r'C:\issue74 runtime libs'
	flags := ['-Wl,-rpath,${rpath}', 'Version.lib']
	expected := ['"-Wl,-rpath,${rpath}"', '-lVersion']
	for compiler_type in [pref.CompilerType.gcc, .mingw] {
		args := issue74_ordered_pkgconfig_args_for_target(flags, .windows, compiler_type)
		assert args == expected, '${compiler_type}: ${args}'
	}
}

fn test_windows_gnu_static_pkgconfig_preserves_quoted_direct_lib_in_direct_link() {
	flags, direct_lib := issue74_quoted_direct_import_lib_case()
	mut builder := issue74_pkgconfig_builder_for_target(flags, .windows, .gcc)
	_, args, _ := builder.split_ordered_pkgconfig_link_flags([])
	assert args.len == 3, args.str()
	assert args[1] == '"${direct_lib}"', args.str()
	assert args[2] == '-lIssue74Mixed', args.str()
	direct_command := args.map(shell_safe_cc_arg(it)).join(' ')
	assert direct_command.contains(' "${direct_lib}" '), direct_command
}

fn test_windows_gnu_static_pkgconfig_preserves_quoted_direct_lib_in_response_file() {
	flags, direct_lib := issue74_quoted_direct_import_lib_case()
	mut builder := issue74_pkgconfig_builder_for_target(flags, .windows, .gcc)
	_, args, _ := builder.split_ordered_pkgconfig_link_flags([])
	rsp_args := args.map(builder.rsp_safe_arg(it))
	assert builder.should_use_rsp(rsp_args)
	response_file_content := rsp_args.join(' ').replace('\\', '\\\\')
	escaped_direct_lib := direct_lib.replace('\\', '\\\\')
	assert response_file_content.contains(' "${escaped_direct_lib}" '), response_file_content
}

fn issue74_is_xlinker_operand_marker(arg string) bool {
	return arg in issue74_xlinker_operand_markers
}

fn test_consecutive_ordinary_xlinker_operand_stays_adjacent_to_its_control() {
	mut builder := issue74_builder_for_source(issue74_xlinker_operand_source)
	all_args := builder.all_args(builder.ccoptions)
	markers := all_args.filter(issue74_is_xlinker_operand_marker(it))
	assert markers == issue74_xlinker_operand_markers, all_args.str()
	operand_index := all_args.index('--issue74-xlinker-operand')
	assert all_args.count(it == '--issue74-xlinker-operand') == 1, all_args.str()
	assert operand_index > 0, all_args.str()
	assert all_args[operand_index - 1] == '-Xlinker', all_args.str()
}

fn test_wl_operand_aliases_keep_their_next_ordinary_flag_adjacent() {
	for control, operand in {
		'-Wl,--rpath':      '/issue74/rpath-long'
		'-Wl,-R':           '/issue74/r-capital'
		'-Wl,-rpath-link':  '/issue74/rpath-link-single'
		'-Wl,--rpath-link': '/issue74/rpath-link'
	} {
		mut builder :=
			issue74_builder_for_source(issue74_wl_operand_source(control, operand, false))
		all_args := builder.all_args(builder.ccoptions)
		control_index := all_args.index(control)
		assert control_index >= 0, '${control}: ${all_args}'
		assert all_args[control_index + 1] == operand, '${control}: ${all_args}'
		assert all_args.index('-Wl,--issue74-order-pkg-public') > control_index + 1, '${control}: ${all_args}'
	}
}

fn test_wl_non_operand_controls_remain_non_consuming() {
	mut table := ast.new_table()
	for control in ['-Wl,--rpath,', '-Wl,--as-needed'] {
		table.parse_cflag_with_link_segment(control, 'main', []) or { panic(err) }
		flag := table.link_flag_segments.last().flags[0]
		assert !ordinary_flag_takes_linker_operand(flag), control
	}
}

fn issue74_builder_with_pkgconfig_pthread(pkgconfig_flags []string, extra_args []string) Builder {
	return issue74_builder_with_pkgconfig_pthread_and_compiler(pkgconfig_flags, 'gcc', extra_args)
}

fn issue74_builder_with_pkgconfig_pthread_and_compiler(pkgconfig_flags []string, ccompiler string, extra_args []string) Builder {
	driver_environment := issue74_clear_driver_environment()
	defer {
		driver_environment.restore()
	}
	return issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment(pkgconfig_flags,
		ccompiler, extra_args)
}

fn issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment(pkgconfig_flags []string, ccompiler string, extra_args []string) Builder {
	source_path := os.join_path('/virtual', 'issue74_pkgconfig_pthread.v')
	mut args := ['', '-cc', ccompiler]
	args << extra_args
	args << source_path
	mut prefs, _ := pref.parse_args([], args)
	prefs.out_name = os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_pthread.out')
	mut table := ast.new_table()
	if pkgconfig_flags.len > 0 {
		table.parse_pkgconfig_link_flags(pkgconfig_flags, 'main', prefs.compile_defines_all) or {
			panic(err)
		}
	}
	mut builder := new_builder(prefs)
	builder.table = table
	builder.out_name_c = os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_pthread.tmp.c')
	builder.setup_ccompiler_options(ccompiler)
	builder.setup_output_name()
	return builder
}

fn test_pkgconfig_builder_helpers_ignore_ambient_driver_flags() {
	driver_environment := issue74_clear_driver_environment()
	defer {
		driver_environment.restore()
	}
	os.setenv('CFLAGS', '-pthread', true)
	os.setenv('LDFLAGS', '-pthread', true)

	builder := issue74_builder_with_pkgconfig_pthread([], [])
	assert builder.get_compile_args().count(it == '-pthread') == 0
	assert builder.get_linker_args().count(it == '-pthread') == 0
	assert os.getenv('CFLAGS') == '-pthread'
	assert os.getenv('LDFLAGS') == '-pthread'
}

fn test_pkgconfig_pthread_provenance_excludes_linker_only_sources() {
	driver_environment := issue74_clear_driver_environment()
	defer {
		driver_environment.restore()
	}

	pkgconfig_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [])
	assert pkgconfig_builder.has_pkgconfig_pthread()
	assert pkgconfig_builder.get_linker_args().count(it == '-pthread') == 1

	ldflags_builder := issue74_builder_with_pkgconfig_pthread([], ['-ldflags', '-pthread'])
	assert !ldflags_builder.has_pkgconfig_pthread()
	assert ldflags_builder.get_linker_args().count(it.trim_space() == '-pthread') == 1

	os.setenv('LDFLAGS', '-pthread', true)
	env_ldflags_builder := issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment([],
		'gcc', [])
	assert !env_ldflags_builder.has_pkgconfig_pthread()
	assert env_ldflags_builder.get_linker_args().count(it == '-pthread') == 1
}

fn test_pkgconfig_pthread_is_projected_once_for_gnu_compile_only_paths() {
	for ccompiler in ['gcc', 'clang', 'x86_64-w64-mingw32-gcc'] {
		object_builder := issue74_builder_with_pkgconfig_pthread_and_compiler([
			'-pthread',
		], ccompiler, ['-is_o'])
		assert object_builder.get_compile_args().count(it == '-pthread') == 1, ccompiler
		assert object_builder.get_linker_args() == [], ccompiler

		module_builder := issue74_builder_with_pkgconfig_pthread_and_compiler([
			'-pthread',
		], ccompiler, ['build-module'])
		assert module_builder.get_compile_args().count(it == '-pthread') == 1, ccompiler
		assert module_builder.get_linker_args() == [], ccompiler
	}
}

fn test_pkgconfig_pthread_is_projected_for_gnu_cplusplus_object_compilation() {
	object_builder := issue74_builder_with_pkgconfig_pthread_and_compiler(['-pthread'], 'g++', [
		'-is_o',
	])
	assert object_builder.pref.ccompiler_type == .cplusplus
	assert object_builder.ccoptions.cc == .unknown
	compile_args := object_builder.get_compile_args()
	assert compile_args.count(it == '-pthread') == 1
	assert object_builder.get_linker_args() == []
	rsp_args :=
		object_builder.all_args(object_builder.ccoptions).map(object_builder.rsp_safe_arg(it))
	assert object_builder.should_use_rsp(rsp_args)
	assert rsp_args.count(it == '-pthread') == 1
}

fn test_pkgconfig_pthread_compile_only_projection_is_deduplicated() {
	object_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [
		'-is_o',
		'-cflags',
		'-pthread',
	])
	compile_args := object_builder.get_compile_args()
	assert compile_args.filter(pref.contains_exact_cflag_token(it, '-pthread')).len == 1
}

fn test_pkgconfig_pthread_combined_cflags_is_not_duplicated_for_compile_only_paths() {
	combined_cflags := '-DISSUE74_CFLAGS=1 -pthread'
	object_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [
		'-is_o',
		'-cflags',
		combined_cflags,
	])
	object_compile_args := object_builder.get_compile_args()
	assert object_compile_args.filter(pref.contains_exact_cflag_token(it, '-pthread')).len == 1

	module_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [
		'build-module',
		'-cflags',
		combined_cflags,
	])
	module_compile_args := module_builder.get_compile_args()
	assert module_compile_args.filter(pref.contains_exact_cflag_token(it, '-pthread')).len == 1
}

fn test_pkgconfig_pthread_combined_environment_cflags_is_not_duplicated_for_compile_only_paths() {
	driver_environment := issue74_clear_driver_environment()
	combined_cflags := '-DISSUE74_ENV_CFLAGS=1 -pthread'
	os.setenv('CFLAGS', combined_cflags, true)
	defer {
		driver_environment.restore()
	}

	object_builder := issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment([
		'-pthread',
	], 'gcc', ['-is_o'])
	object_compile_args := object_builder.get_compile_args()
	assert object_compile_args.filter(pref.contains_exact_cflag_token(it, '-pthread')).len == 1

	module_builder := issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment([
		'-pthread',
	], 'gcc', ['build-module'])
	module_compile_args := module_builder.get_compile_args()
	assert module_compile_args.filter(pref.contains_exact_cflag_token(it, '-pthread')).len == 1
}

fn test_pkgconfig_pthread_direct_and_unsupported_compilers_are_not_projected() {
	direct_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [])
	assert direct_builder.get_compile_args().count(it == '-pthread') == 0
	assert direct_builder.get_linker_args().count(it == '-pthread') == 1
	assert direct_builder.all_args(direct_builder.ccoptions).count(it == '-pthread') == 1

	shared_module_builder := issue74_builder_with_pkgconfig_pthread(['-pthread'], [
		'-shared',
		'build-module',
	])
	assert shared_module_builder.get_compile_args().count(it == '-pthread') == 0
	assert shared_module_builder.get_linker_args().count(it == '-pthread') == 1

	for ccompiler in ['tcc', 'msvc'] {
		object_builder := issue74_builder_with_pkgconfig_pthread_and_compiler([
			'-pthread',
		], ccompiler, ['-is_o'])
		assert object_builder.get_compile_args().count(it == '-pthread') == 0, ccompiler
	}
}

fn test_linker_only_pthread_is_not_projected_for_object_compilation() {
	driver_environment := issue74_clear_driver_environment()
	defer {
		driver_environment.restore()
	}

	explicit_builder := issue74_builder_with_pkgconfig_pthread([], [
		'-is_o',
		'-ldflags',
		'-pthread',
	])
	assert explicit_builder.get_compile_args().count(it == '-pthread') == 0

	os.setenv('LDFLAGS', '-pthread', true)
	environment_builder := issue74_builder_with_pkgconfig_pthread_and_compiler_from_environment([],
		'gcc', ['-is_o'])
	assert environment_builder.get_compile_args().count(it == '-pthread') == 0
}

fn issue74_environment_value_case_insensitive(env map[string]string, name string) string {
	lower_name := name.to_lower_ascii()
	mut fallback := ''
	for key, value in env {
		if key == name {
			return value
		}
		if key.to_lower_ascii() == lower_name {
			fallback = value
		}
	}
	return fallback
}

fn issue74_replace_environment_value_case_insensitive(mut env map[string]string, name string, value string) {
	lower_name := name.to_lower_ascii()
	for key in env.keys() {
		if key.to_lower_ascii() == lower_name {
			env.delete(key)
		}
	}
	env[name] = value
}

fn issue74_remove_driver_environment(mut env map[string]string) {
	for key in env.keys() {
		if key.to_upper_ascii() in ['CFLAGS', 'LDFLAGS'] {
			env.delete(key)
		}
	}
}

fn issue74_write_fake_ccompiler(path string) {
	$if windows {
		bin_dir := os.dir(path)
		recorder_source := os.join_path(bin_dir, 'issue74_fake_ccompiler.v')
		recorder_exe := os.join_path(bin_dir, 'issue74_fake_ccompiler.exe')
		if !os.exists(recorder_exe) {
			os.write_file(recorder_source, issue74_windows_fake_ccompiler_source) or { panic(err) }
			mut process := os.new_process(@VEXE)
			process.set_args(['-gc', 'none', '-no-retry-compilation', '-o', recorder_exe,
				recorder_source])
			process.set_work_folder(@VEXEROOT)
			mut recorder_env := os.environ()
			issue74_remove_driver_environment(mut recorder_env)
			issue74_replace_environment_value_case_insensitive(mut recorder_env, 'VFLAGS', '')
			process.set_environment(recorder_env)
			process.set_redirect_stdio()
			process.wait()
			stdout := process.stdout_slurp()
			stderr := process.stderr_slurp()
			exit_code := process.code
			process.close()
			assert exit_code == 0, 'failed to build Windows fake C compiler:\nstdout:\n${stdout}\nstderr:\n${stderr}'
		}
		if !os.exists(path) {
			os.cp(recorder_exe, path) or { panic(err) }
		}
		return
	}
	os.write_file(path, '#!/bin/sh
set -eu
: "\${ISSUE74_CC_LOG:?ISSUE74_CC_LOG must name the argv log}"
for arg do
	printf "%s\\n" "\$arg" >> "\$ISSUE74_CC_LOG"
done
output=""
take_output=0
for arg do
	if [ "\$take_output" -eq 1 ]; then
		output=\$arg
		take_output=0
	elif [ "\$arg" = "-o" ]; then
		take_output=1
	fi
done
if [ -n "\$output" ]; then
	mkdir -p "\$(dirname "\$output")"
	: > "\$output"
fi
') or {
		panic(err)
	}
	os.chmod(path, 0o700) or { panic(err) }
}

struct Issue74FakeCompileResult {
	exit_code int
	stdout    string
	stderr    string
	argv      []string
}

fn issue74_run_fake_compile(root string, ccompiler string, name string, source string) Issue74FakeCompileResult {
	return issue74_run_fake_compile_with_args(root, ccompiler, name, source, []string{})
}

fn issue74_run_fake_compile_with_args(root string, ccompiler string, name string, source string, extra_args []string) Issue74FakeCompileResult {
	bin_dir := os.join_path(root, 'bin')
	probe_name := '${ccompiler}_${name}'
	tmp_dir := os.join_path(root, 'tmp', probe_name)
	os.mkdir_all(bin_dir) or { panic(err) }
	os.mkdir_all(tmp_dir) or { panic(err) }
	mut fake_ccompiler_name := ccompiler
	$if windows {
		fake_ccompiler_name += '.exe'
	}
	fake_ccompiler := os.join_path(bin_dir, fake_ccompiler_name)
	issue74_write_fake_ccompiler(fake_ccompiler)
	source_path := os.join_path(root, '${probe_name}.v')
	output_path := os.join_path(root, '${probe_name}.out')
	log_path := os.join_path(root, '${probe_name}.argv')
	os.write_file(source_path, source) or { panic(err) }
	mut env := os.environ()
	issue74_remove_driver_environment(mut env)
	current_path := issue74_environment_value_case_insensitive(env, 'PATH')
	issue74_replace_environment_value_case_insensitive(mut env, 'PATH',
		'${bin_dir}${os.path_delimiter}${current_path}')
	env['PKG_CONFIG_PATH'] = ''
	env['PKG_CONFIG_PATH_DEFAULTS'] = issue74_fixture_dir
	env['ISSUE74_CC_LOG'] = log_path
	env['TMPDIR'] = tmp_dir
	issue74_replace_environment_value_case_insensitive(mut env, 'VFLAGS', '')
	mut process := os.new_process(@VEXE)
	mut compiler_args := ['-no-rsp', '-gc', 'none', '-cc', ccompiler, '-no-retry-compilation']
	compiler_args << extra_args
	compiler_args << ['-o', output_path, source_path]
	process.set_args(compiler_args)
	process.set_work_folder(@VEXEROOT)
	process.set_environment(env)
	process.set_redirect_stdio()
	process.wait()
	stdout := process.stdout_slurp()
	stderr := process.stderr_slurp()
	exit_code := process.code
	process.close()
	argv := if os.exists(log_path) { os.read_lines(log_path) or { panic(err) } } else { []string{} }
	return Issue74FakeCompileResult{
		exit_code: exit_code
		stdout:    stdout
		stderr:    stderr
		argv:      argv
	}
}

fn issue74_fake_compile(root string, ccompiler string, name string, source string) []string {
	result := issue74_run_fake_compile(root, ccompiler, name, source)
	assert result.exit_code == 0, 'fake ${ccompiler} probe ${name} failed:\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}'
	return result.argv
}

fn issue74_fake_compile_with_args(root string, ccompiler string, name string, source string, extra_args []string) []string {
	result := issue74_run_fake_compile_with_args(root, ccompiler, name, source, extra_args)
	assert result.exit_code == 0, 'fake ${ccompiler} probe ${name} failed:\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}'
	return result.argv
}

fn issue74_is_fake_compiler_marker(arg string) bool {
	return arg.contains('ISSUE74_') || arg.starts_with('-Wl,--issue74-')
}

fn issue74_is_static_linker_marker(arg string) bool {
	return arg.starts_with('-lissue74_') || arg in ['-Wl,--start-group', '-Wl,--end-group']
}

fn issue74_is_group_alias_marker(arg string) bool {
	return arg.starts_with('-lissue74_alias_') || arg in ['-Wl,-(', '-Wl,-)']
}

fn test_shell_safe_cc_arg_only_quotes_group_aliases() {
	assert shell_safe_cc_arg('-Wl,-(') == os.quoted_path('-Wl,-(')
	assert shell_safe_cc_arg('-Wl,-)') == os.quoted_path('-Wl,-)')
	for arg in ['-pthread', '-lissue74_unchanged', '-Wl,--start-group', '-Wl,--end-group',
		'-DISSUE74_UNCHANGED=1'] {
		assert shell_safe_cc_arg(arg) == arg
	}
}

fn test_pkgconfig_posix_fake_compiler_final_argv() {
	$if linux || macos {
		root := os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_fake_compiler')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		for ccompiler in ['gcc', 'clang'] {
			order_markers := issue74_fake_compile(root, ccompiler, 'source_order',
				issue74_source_order_source).filter(issue74_is_source_order_marker(it))
			assert order_markers == issue74_source_order_markers, ccompiler
			dynamic_argv := issue74_fake_compile(root, ccompiler, 'dynamic_compat',
				issue74_dynamic_compat_source)
			dynamic_define_index := dynamic_argv.index('ISSUE74_DYNAMIC_SENTINEL')
			assert dynamic_define_index > 0, ccompiler
			assert dynamic_argv[dynamic_define_index - 1] == '-D', ccompiler
			dynamic_markers := dynamic_argv.filter(issue74_is_fake_compiler_marker(it))
			assert dynamic_markers == [
				'ISSUE74_DYNAMIC_SENTINEL',
				'-Wl,--issue74-dynamic-sentinel',
			], ccompiler
			xlinker_argv := issue74_fake_compile(root, ccompiler, 'xlinker_operand',
				issue74_xlinker_operand_source)
			xlinker_markers := xlinker_argv.filter(issue74_is_xlinker_operand_marker(it))
			assert xlinker_markers == issue74_xlinker_operand_markers, ccompiler
			xlinker_operand_index := xlinker_argv.index('--issue74-xlinker-operand')
			assert xlinker_argv.count(it == '--issue74-xlinker-operand') == 1, ccompiler
			assert xlinker_operand_index > 0, ccompiler
			assert xlinker_argv[xlinker_operand_index - 1] == '-Xlinker', ccompiler

			ldflags_static_argv := issue74_fake_compile_with_args(root, ccompiler,
				'ldflags_static', issue74_ldflags_static_source, ['-ldflags', '-static'])
			static_markers := ldflags_static_argv.filter(issue74_is_static_linker_marker(it))
			assert static_markers == issue74_static_linker_markers, ccompiler
			assert ldflags_static_argv.count(it == '-static') == 1, ccompiler

			group_alias_argv := issue74_fake_compile(root, ccompiler, 'group_alias',
				issue74_group_alias_source)
			group_alias_markers := group_alias_argv.filter(issue74_is_group_alias_marker(it))
			assert group_alias_markers == issue74_group_alias_markers, ccompiler

			parallel_group_alias_argv := issue74_fake_compile_with_args(root, ccompiler,
				'group_alias_parallel', issue74_group_alias_source, ['-parallel-cc'])
			parallel_group_alias_markers :=
				parallel_group_alias_argv.filter(issue74_is_group_alias_marker(it))
			assert parallel_group_alias_markers == issue74_group_alias_markers, ccompiler
		}
	}
}

fn test_pkgconfig_posix_generated_project_group_alias_commands() {
	$if linux || macos {
		root := os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_generated_project_group_alias')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		project_dir := os.join_path(root, 'project')
		unrelated_arg := '-DISSUE74_GENERATED_PROJECT_UNCHANGED=1'
		generated := issue74_run_fake_compile_with_args(root, 'gcc', 'group_alias_project',
			issue74_group_alias_source, [
			'-cflags',
			unrelated_arg,
			'-generate-c-project',
			project_dir,
		])
		assert generated.exit_code == 0, 'generated project probe failed:\nstdout:\n${generated.stdout}\nstderr:\n${generated.stderr}'

		quoted_group_start := os.quoted_path('-Wl,-(')
		quoted_group_end := os.quoted_path('-Wl,-)')
		for build_file in ['build.sh', 'Makefile'] {
			command := os.read_file(os.join_path(project_dir, build_file)) or { panic(err) }
			assert command.contains(quoted_group_start), build_file
			assert command.contains(quoted_group_end), build_file
			assert command.contains(unrelated_arg), build_file
			assert !command.contains(' -Wl,-( '), build_file
			assert !command.contains(' -Wl,-) '), build_file
		}

		bin_dir := os.join_path(root, 'bin')
		mut env := os.environ()
		issue74_remove_driver_environment(mut env)
		current_path := issue74_environment_value_case_insensitive(env, 'PATH')
		issue74_replace_environment_value_case_insensitive(mut env, 'PATH',
			'${bin_dir}${os.path_delimiter}${current_path}')
		issue74_replace_environment_value_case_insensitive(mut env, 'VFLAGS', '')
		for process_args in [
			['sh', os.join_path(project_dir, 'build.sh')],
			['make'],
		] {
			log_path := os.join_path(root, '${process_args[0]}.generated.argv')
			os.rm(log_path) or {}
			env['ISSUE74_CC_LOG'] = log_path
			mut process := os.new_process(process_args[0])
			process.set_args(process_args[1..])
			process.set_work_folder(project_dir)
			process.set_environment(env)
			process.set_redirect_stdio()
			process.wait()
			stdout := process.stdout_slurp()
			stderr := process.stderr_slurp()
			exit_code := process.code
			process.close()
			assert exit_code == 0, '${process_args[0]} generated project build failed:\nstdout:\n${stdout}\nstderr:\n${stderr}'
			argv := os.read_lines(log_path) or { panic(err) }
			group_alias_markers := argv.filter(issue74_is_group_alias_marker(it))
			assert group_alias_markers == issue74_group_alias_markers, process_args[0]
			assert argv.count(it == unrelated_arg) == 1, process_args[0]
		}
	}
}

fn test_pkgconfig_windows_fake_compiler_group_alias_final_argv() {
	$if windows {
		root := os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_windows_fake_compiler')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		for ccompiler in ['gcc', 'clang'] {
			group_alias_argv := issue74_fake_compile(root, ccompiler, 'group_alias_windows',
				issue74_group_alias_source)
			group_alias_markers := group_alias_argv.filter(issue74_is_group_alias_marker(it))
			assert group_alias_markers == issue74_group_alias_markers, ccompiler

			parallel_group_alias_argv := issue74_fake_compile_with_args(root, ccompiler,
				'group_alias_parallel_windows', issue74_group_alias_source, [
				'-parallel-cc',
			])
			parallel_group_alias_markers :=
				parallel_group_alias_argv.filter(issue74_is_group_alias_marker(it))
			assert parallel_group_alias_markers == issue74_group_alias_markers, ccompiler
		}
	}
}

fn test_pkgconfig_posix_builder_rejects_incomplete_xlinker_operand() {
	$if linux || macos {
		root := os.join_path(os.vtmp_dir(), 'issue74_pkgconfig_incomplete_xlinker')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		for ccompiler in ['gcc', 'clang'] {
			interrupted := issue74_run_fake_compile(root, ccompiler, 'xlinker_interrupted',
				issue74_xlinker_interrupted_source)
			assert interrupted.exit_code != 0, ccompiler
			assert interrupted.argv == [], ccompiler
			assert (interrupted.stdout + interrupted.stderr).contains('incomplete linker option `-Xlinker` before `#pkgconfig`'), ccompiler

			end_of_stream := issue74_run_fake_compile(root, ccompiler, 'xlinker_end_of_stream',
				issue74_xlinker_end_of_stream_source)
			assert end_of_stream.exit_code != 0, ccompiler
			assert end_of_stream.argv == [], ccompiler
			assert (end_of_stream.stdout + end_of_stream.stderr).contains('incomplete linker option `-Xlinker` at the end of ordered linker flags'), ccompiler

			rpath_interrupted := issue74_run_fake_compile(root, ccompiler, 'wl_rpath_interrupted',
				issue74_wl_rpath_interrupted_source)
			assert rpath_interrupted.exit_code != 0, ccompiler
			assert rpath_interrupted.argv == [], ccompiler
			assert (rpath_interrupted.stdout + rpath_interrupted.stderr).contains('incomplete linker option `-Wl,-rpath` before `#pkgconfig`'), ccompiler

			version_script_interrupted := issue74_run_fake_compile(root, ccompiler,
				'version_script_interrupted', issue74_version_script_interrupted_source)
			assert version_script_interrupted.exit_code != 0, ccompiler
			assert version_script_interrupted.argv == [], ccompiler
			assert (version_script_interrupted.stdout + version_script_interrupted.stderr).contains('incomplete linker option `-Wl,--version-script` before `#pkgconfig`'), ccompiler

			for control, operand in {
				'-Wl,--rpath':      '/issue74/rpath-long'
				'-Wl,-R':           '/issue74/r-capital'
				'-Wl,-rpath-link':  '/issue74/rpath-link-single'
				'-Wl,--rpath-link': '/issue74/rpath-link'
			} {
				alias := control.replace_each(['-', '_', ',', '_'])
				alias_interrupted := issue74_run_fake_compile(root, ccompiler, alias, issue74_wl_operand_source(control,
					operand, true))
				assert alias_interrupted.exit_code != 0, '${ccompiler}: ${control}'
				assert alias_interrupted.argv == [], '${ccompiler}: ${control}'
				assert (alias_interrupted.stdout + alias_interrupted.stderr).contains('incomplete linker option `${control}` before `#pkgconfig`'), '${ccompiler}: ${control}'
			}
		}
	}
}
