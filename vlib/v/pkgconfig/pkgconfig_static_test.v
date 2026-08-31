import os
import v.pkgconfig

const issue74_fixture_dir = os.join_path(@VEXEROOT, 'vlib', 'v', 'pkgconfig', 'testdata',
	'static_pkgconfig')

fn issue74_pkgconfig_result(args []string) pkgconfig.MainResult {
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
	mut command := pkgconfig.main(args) or { panic(err) }
	return command.run_result() or { panic(err) }
}

fn issue74_pkgconfig_tokens(args []string) []string {
	return issue74_pkgconfig_result(args).output.fields()
}

struct Issue74PkgconfigToolResult {
	exit_code int
	stdout    string
	stderr    string
}

fn issue74_pkgconfig_run_tool(executable string, args []string, work_dir string) Issue74PkgconfigToolResult {
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_redirect_stdio()
	process.wait()
	result := Issue74PkgconfigToolResult{
		exit_code: process.code
		stdout:    process.stdout_slurp()
		stderr:    process.stderr_slurp()
	}
	process.close()
	return result
}

struct Issue74StaticCase {
	package string
	libs    []string
	cflags  []string
}

struct Issue74SplitLinkerCase {
	control   string
	package_a string
	package_b string
	operand_a string
	operand_b string
}

fn issue74_split_linker_cases() []Issue74SplitLinkerCase {
	return [
		Issue74SplitLinkerCase{
			control:   '-Wl,-rpath'
			package_a: 'split-rpath-a-74'
			package_b: 'split-rpath-b-74'
			operand_a: '-Wl,/issue74/split-rpath-a'
			operand_b: '-Wl,/issue74/split-rpath-b'
		},
		Issue74SplitLinkerCase{
			control:   '-Wl,--rpath'
			package_a: 'split-rpath-long-a-74'
			package_b: 'split-rpath-long-b-74'
			operand_a: '-Wl,/tmp'
			operand_b: '-Wl,/var/tmp'
		},
		Issue74SplitLinkerCase{
			control:   '-Wl,-R'
			package_a: 'split-rpath-capital-r-a-74'
			package_b: 'split-rpath-capital-r-b-74'
			operand_a: '-Wl,/tmp'
			operand_b: '-Wl,/var/tmp'
		},
		Issue74SplitLinkerCase{
			control:   '-Wl,-rpath-link'
			package_a: 'split-rpath-link-a-74'
			package_b: 'split-rpath-link-b-74'
			operand_a: '-Wl,/tmp'
			operand_b: '-Wl,/var/tmp'
		},
		Issue74SplitLinkerCase{
			control:   '-Wl,--rpath-link'
			package_a: 'split-rpath-link-long-a-74'
			package_b: 'split-rpath-link-long-b-74'
			operand_a: '-Wl,/tmp'
			operand_b: '-Wl,/var/tmp'
		},
	]
}

fn issue74_assert_static_case(case Issue74StaticCase) {
	libs_args := ['--static', '--libs', case.package]
	cflags_args := ['--static', '--cflags', case.package]
	assert issue74_pkgconfig_tokens(libs_args) == case.libs
	assert issue74_pkgconfig_tokens(cflags_args) == case.cflags
}

fn test_pkgconfig_dynamic_keeps_historical_private_dependency_overlinking() {
	assert issue74_pkgconfig_tokens(['--libs', 'static-root-74']) == [
		'-lissue74_root',
		'-lissue74_public',
		'-lissue74_private',
	]
	assert issue74_pkgconfig_tokens(['--cflags', 'static-root-74']) == [
		'-DISSUE74_ROOT',
		'-DISSUE74_PUBLIC',
		'-DISSUE74_PRIVATE',
	]
}

fn test_pkgconfig_dynamic_repeated_fields_keep_historical_last_wins() {
	assert issue74_pkgconfig_tokens(['--cflags', 'repeated-static-fields-74']) == [
		'-DISSUE74_REPEATED_ROOT_PUBLIC_B',
		'-DORACLE_DIAMOND_COMMON',
		'-DISSUE74_UPPERCASE_PROVIDER',
	]
	assert issue74_pkgconfig_tokens(['--libs', 'repeated-static-fields-74']) == [
		'-lissue74_repeated_root_public_b',
		'-loracle_diamond_common',
		'-luppercase_private_provider',
	]
}

fn test_pkgconfig_static_repeated_fields_accumulate_in_source_order() {
	assert issue74_pkgconfig_tokens([
		'--static',
		'--print-requires',
		'repeated-static-fields-74',
	]) == ['static-public-74', 'oracle-diamond-common-74']

	issue74_assert_static_case(Issue74StaticCase{
		package: 'repeated-static-fields-74'
		libs:    ['-lissue74_repeated_root_public_a', '-lissue74_repeated_root_public_b',
			'-lissue74_repeated_root_private_a', '-lissue74_repeated_root_private_b',
			'-lissue74_public', '-lissue74_public_private', '-loracle_diamond_common',
			'-lissue74_private', '-lissue74_private_private', '-luppercase_private_provider']
		cflags:  ['-DISSUE74_REPEATED_ROOT_PUBLIC_A', '-DISSUE74_REPEATED_ROOT_PUBLIC_B',
			'-DISSUE74_PUBLIC', '-DORACLE_DIAMOND_COMMON', '-DISSUE74_PRIVATE',
			'-DISSUE74_UPPERCASE_PROVIDER', '-DISSUE74_REPEATED_ROOT_PRIVATE_A',
			'-DISSUE74_REPEATED_ROOT_PRIVATE_B', '-DISSUE74_PUBLIC_PRIVATE',
			'-DISSUE74_PRIVATE_PRIVATE', '-DFRIBIDI_LIB_STATIC']
	})
}

fn test_pkgconfig_static_matches_reference_closure_order_and_repetition() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'static-root-74'
		libs:    ['-lissue74_root', '-Wl,--start-group', '-lissue74_cycle_a', '-lissue74_cycle_b',
			'-lissue74_cycle_a', '-Wl,--end-group', '-lissue74_root_private', '-lissue74_public',
			'-lissue74_public_private', '-lissue74_private', '-lissue74_private_private']
		cflags:  ['-DISSUE74_ROOT', '-DISSUE74_PUBLIC', '-DISSUE74_PRIVATE', '-DISSUE74_ROOT_PRIVATE',
			'-DISSUE74_PUBLIC_PRIVATE', '-DISSUE74_PRIVATE_PRIVATE']
	})
}

fn test_pkgconfig_static_public_duplicate_graph_preserves_expected_order() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'oracle-duplicate-root-74'
		libs:    ['-loracle_duplicate_root', '-loracle_duplicate_left', '-loracle_duplicate_shared',
			'-loracle_duplicate_right']
		cflags:  ['-DORACLE_DUPLICATE_ROOT', '-DORACLE_DUPLICATE_LEFT', '-DORACLE_DUPLICATE_SHARED',
			'-DORACLE_DUPLICATE_RIGHT']
	})
}

fn test_pkgconfig_static_framework_search_paths_keep_first_occurrence_order() {
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'framework-search-path-74',
	]) == ['-F/sdk1', '-F/sdk2', '-framework', 'Foo']
}

fn test_pkgconfig_static_preserves_public_split_link_pairs() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'split-link-pairs-root-74',
	]).link_flags == [
		'-L',
		'/issue74/root-search',
		'-l',
		'issue74_root',
		'-L',
		'/issue74/search-a',
		'-l',
		'issue74_a',
		'-L',
		'/issue74/search-b',
		'-l',
		'issue74_b',
	]
}

fn test_pkgconfig_static_tokenizes_quoted_and_escaped_split_link_operands() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'quoted-split-link-pairs-74',
	]).link_flags == [
		'-L',
		'/issue74/public search',
		'-l',
		'issue74_public',
		'-L',
		'/issue74/private search',
		'-l',
		'issue74_private',
	]
}

fn test_pkgconfig_static_keeps_quoted_expanded_space_variable_in_one_link_fragment() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'expanded-space-link-74',
	]).link_flags == [
		'-L',
		'/issue74/expanded lib',
		'-l',
		'issue74_expanded_space',
		'/issue74/expanded lib/libissue74_expanded_direct.a',
	]
}

fn test_pkgconfig_static_retokenizes_unquoted_expanded_link_flags() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'expanded-link-flags-74',
	]).link_flags == ['-lissue74_expanded_a', '-lissue74_expanded_b']
	// The historical dynamic parser does not accumulate space-containing variable assignments.
	assert issue74_pkgconfig_tokens(['--libs', 'expanded-link-flags-74']) == []
}

fn test_pkgconfig_static_folds_continued_physical_lines() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'continued-static-fields-74',
	]).link_flags == [
		'-L/issue74/continued',
		'-lissue74_cont_public_a',
		'-lissue74_cont_public_b',
		'-lissue74_cont_var_a',
		'-lissue74_cont_var_b',
		'-lissue74_cont_private_a',
		'-lissue74_cont_private_b',
	]
	pc := pkgconfig.load('continued-static-fields-74', pkgconfig.Options{
		path:              issue74_fixture_dir
		use_default_paths: false
		link_mode:         .static_
	}) or { panic(err) }
	assert pc.vars['odd_backslashes'] == 'left\\\\right'
	assert pc.vars['even_backslashes'] == 'left\\\\'
	assert issue74_pkgconfig_tokens(['--libs', 'continued-static-fields-74']) == [
		'-L/issue74/continued',
		'-lissue74_cont_public_a',
		'\\',
	]

	crlf_fixture := os.join_path(os.vtmp_dir(), 'continued-static-crlf-74-${os.getpid()}.pc')
	os.write_file(crlf_fixture,
		'Name: continued-static-crlf-74\r\nDescription: CRLF continuation probe\r\nVersion: 1.0.0\r\nLibs: -lissue74_crlf_a \\\r\n\t-lissue74_crlf_b\r\n') or {
		panic(err)
	}
	defer {
		os.rm(crlf_fixture) or {}
	}
	mut command := pkgconfig.main(['--static', '--libs', crlf_fixture]) or { panic(err) }
	result := command.run_result() or { panic(err) }
	assert result.link_flags == ['-lissue74_crlf_a', '-lissue74_crlf_b']
}

fn test_pkgconfig_static_consumes_odd_continuation_marker_at_eof() {
	prefix := 'Name: continued-static-eof-74\nDescription: EOF continuation probe\nVersion: 1.0.0\nLibs: -lissue74_eof '
	contents := [
		prefix + '\\',
		prefix + '\\\n',
		prefix + '\\\\',
		prefix + '\\\\\n',
	]
	for i, content in contents {
		fixture := os.join_path(os.vtmp_dir(), 'continued-static-eof-74-${os.getpid()}-${i}.pc')
		os.write_file(fixture, content) or { panic(err) }
		mut command := pkgconfig.main(['--static', '--libs', fixture]) or { panic(err) }
		result := command.run_result() or { panic(err) }
		os.rm(fixture) or { panic(err) }
		expected := if i < 2 { ['-lissue74_eof'] } else { ['-lissue74_eof', '\\'] }
		assert result.link_flags == expected
	}
}

fn test_pkgconfig_static_accepts_spaced_variable_assignments() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'spaced-variable-assignment-74',
	]).link_flags == ['-L/issue74/spaced-assignment', '-lissue74_spaced_assignment']
	pc := pkgconfig.load('spaced-variable-assignment-74', pkgconfig.Options{
		path:              issue74_fixture_dir
		use_default_paths: false
		link_mode:         .static_
	}) or { panic(err) }
	assert pc.vars['libdir'] == '/issue74/spaced-assignment'
	assert pc.vars['embedded_equals'] == 'left=right'
	assert pc.description == 'Spaced assignment=field value'
	assert issue74_pkgconfig_tokens(['--libs', 'spaced-variable-assignment-74']) == [
		'-L',
		'-lissue74_spaced_assignment',
	]

	invalid_fixture := os.join_path(os.vtmp_dir(),
		'static-variable-validation-74-${os.getpid()}.pc')
	os.write_file(invalid_fixture,
		'=empty\nbad key = internal\ntabbed\t=\ttrimmed\nName: static-variable-validation-74\nDescription: field=value\nVersion: 1.0.0\nLibs: -lissue74_validation\n') or {
		panic(err)
	}
	defer {
		os.rm(invalid_fixture) or {}
	}
	invalid_pc := pkgconfig.load(invalid_fixture, pkgconfig.Options{
		use_default_paths: false
		link_mode:         .static_
	}) or { panic(err) }
	assert '' !in invalid_pc.vars
	assert 'bad key' !in invalid_pc.vars
	assert invalid_pc.vars['tabbed'] == 'trimmed'
	assert invalid_pc.description == 'field=value'
}

fn test_pkgconfig_static_keeps_split_version_script_operand_adjacent_when_repeated() {
	shared_operand := '/issue74/shared-symbol-list.map'
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'split-version-script-repeat-74',
	]).link_flags == [
		'-Wl,--version-script',
		'-Wl,${shared_operand}',
		'-Wl,--dynamic-list',
		'-Wl,${shared_operand}',
	]

	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_split_version_script_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_source := os.join_path(root, 'main.c')
		version_script := os.join_path(root, 'symbols.map')
		pc_file := os.join_path(root, 'split-version-script-link-74.pc')
		output := os.join_path(root, 'split-version-script.out')
		os.write_file(main_source,
			'int issue74_exported(void) { return 0; }\nint main(void) { return issue74_exported(); }\n') or {
			panic(err)
		}
		os.write_file(version_script, '{\n\tissue74_exported;\n};\n') or { panic(err) }
		os.write_file(pc_file,
			'Name: split-version-script-link-74\nDescription: Repeated split version-script operand link proof\nVersion: 1.0.0\nLibs: -Wl,--version-script -Wl,${version_script} -Wl,--dynamic-list -Wl,${version_script}\n') or {
			panic(err)
		}
		link_flags := issue74_pkgconfig_result(['--static', '--libs', pc_file]).link_flags
		assert link_flags == [
			'-Wl,--version-script',
			'-Wl,${version_script}',
			'-Wl,--dynamic-list',
			'-Wl,${version_script}',
		]
		mut link_args := [main_source]
		link_args << link_flags
		link_args << ['-o', output]
		linked := issue74_pkgconfig_run_tool(gcc, link_args, root)
		assert linked.exit_code == 0, linked.stdout + linked.stderr
		assert issue74_pkgconfig_run_tool(output, [], root).exit_code == 0
	}
}

fn test_pkgconfig_static_preserves_quoted_public_and_private_link_operands() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'quoted-static-link-operands-74',
	]).link_flags == [
		'/issue74/double  space/libissue74_public.a',
		'/issue74/comma, space/libissue74_private.a',
	]

	$if linux {
		gcc := os.find_abs_path_of_executable('gcc') or { return }
		ar := os.find_abs_path_of_executable('ar') or { return }
		root := os.join_path(os.vtmp_dir(), 'issue74_quoted_link_operands_${os.getpid()}')
		public_dir := os.join_path(root, 'double  space')
		private_dir := os.join_path(root, 'comma, space')
		os.rmdir_all(root) or {}
		os.mkdir_all(public_dir) or { panic(err) }
		os.mkdir_all(private_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_source := os.join_path(root, 'main.c')
		public_source := os.join_path(root, 'public.c')
		private_source := os.join_path(root, 'private.c')
		public_object := os.join_path(root, 'public.o')
		private_object := os.join_path(root, 'private.o')
		public_archive := os.join_path(public_dir, 'libissue74_public.a')
		private_archive := os.join_path(private_dir, 'libissue74_private.a')
		pc_file := os.join_path(root, 'quoted-static-link-operands-link-74.pc')
		output := os.join_path(root, 'quoted-static-link-operands.out')
		os.write_file(main_source,
			'int issue74_public(void);\nint main(void) { return issue74_public() == 42 ? 0 : 1; }\n') or {
			panic(err)
		}
		os.write_file(public_source,
			'int issue74_private(void);\nint issue74_public(void) { return issue74_private() + 1; }\n') or {
			panic(err)
		}
		os.write_file(private_source, 'int issue74_private(void) { return 41; }\n') or {
			panic(err)
		}
		for source, object in {
			public_source:  public_object
			private_source: private_object
		} {
			compiled := issue74_pkgconfig_run_tool(gcc, ['-c', source, '-o', object], root)
			assert compiled.exit_code == 0, compiled.stdout + compiled.stderr
		}
		for archive, object in {
			public_archive:  public_object
			private_archive: private_object
		} {
			archived := issue74_pkgconfig_run_tool(ar, ['rcs', archive, object], root)
			assert archived.exit_code == 0, archived.stdout + archived.stderr
		}
		os.write_file(pc_file,
			'Name: quoted-static-link-operands-link-74\nDescription: Quoted public and private archive link proof\nVersion: 1.0.0\nLibs: "${public_archive}"\nLibs.private: "${private_archive}"\n') or {
			panic(err)
		}
		link_flags := issue74_pkgconfig_result(['--static', '--libs', pc_file]).link_flags
		assert link_flags == [public_archive, private_archive]
		mut link_args := [main_source]
		link_args << link_flags
		link_args << ['-o', output]
		linked := issue74_pkgconfig_run_tool(gcc, link_args, root)
		assert linked.exit_code == 0, linked.stdout + linked.stderr
		assert issue74_pkgconfig_run_tool(output, [], root).exit_code == 0
	}
}

fn test_pkgconfig_static_decodes_escaped_hash_before_normal_comment() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'escaped-hash-link-74',
	]).link_flags == ['-L', '/issue74/escaped#hash', '-lescaped']
}

fn test_pkgconfig_static_rejects_unterminated_link_flag_quote() {
	fixture := os.join_path(issue74_fixture_dir, 'unterminated-link-quote-74.pc')
	mut command := pkgconfig.main(['--static', '--libs', fixture]) or { panic(err) }
	command.run_result() or {
		assert err.msg().contains('unterminated quote in static pkg-config linker flags')
		return
	}
	assert false
}

fn test_pkgconfig_static_preserves_transitive_parser_error_context() {
	pkgconfig.resolve(['transitive-link-error-root-74'], pkgconfig.Options{
		path:              issue74_fixture_dir
		use_default_paths: false
		link_mode:         .static_
	}) or {
		assert err.msg().contains('could not resolve dependency transitive-link-error-middle-74')
		assert err.msg().contains('could not resolve dependency unterminated-link-quote-74')
		assert err.msg().contains('unterminated quote in static pkg-config linker flags')
		return
	}
	assert false
}

fn test_pkgconfig_static_public_diamond_graph_preserves_expected_order() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'oracle-diamond-root-74'
		libs:    ['-loracle_diamond_root', '-loracle_diamond_left', '-loracle_diamond_right',
			'-loracle_diamond_common']
		cflags:  ['-DORACLE_DIAMOND_ROOT', '-DORACLE_DIAMOND_LEFT', '-DORACLE_DIAMOND_RIGHT',
			'-DORACLE_DIAMOND_COMMON']
	})
}

fn test_pkgconfig_static_public_complex_cycle_preserves_expected_order() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'oracle-cycle-root-74'
		libs:    ['-loracle_cycle_root', '-loracle_cycle_a', '-loracle_cycle_b', '-loracle_cycle_c']
		cflags:  ['-DORACLE_CYCLE_ROOT', '-DORACLE_CYCLE_A', '-DORACLE_CYCLE_B', '-DORACLE_CYCLE_C']
	})
}

fn test_pkgconfig_uppercase_private_cflags_are_static_only() {
	dynamic_args := ['--cflags', 'uppercase-private-root-74']
	dynamic_cflags := ['-DISSUE74_UPPERCASE_ROOT', '-DISSUE74_UPPERCASE_PROVIDER']
	assert issue74_pkgconfig_tokens(dynamic_args) == dynamic_cflags

	issue74_assert_static_case(Issue74StaticCase{
		package: 'uppercase-private-root-74'
		libs:    ['-luppercase_private_root', '-luppercase_private_provider']
		cflags:  ['-DISSUE74_UPPERCASE_ROOT', '-DISSUE74_UPPERCASE_PROVIDER', '-DFRIBIDI_LIB_STATIC']
	})
}

fn test_pkgconfig_mixed_case_fields_are_static_only() {
	assert issue74_pkgconfig_tokens([
		'--cflags',
		'mixed-case-dynamic-sentinel-74',
		'mixed-case-static-74',
	]) == ['-DISSUE74_DYNAMIC_SENTINEL']
	assert issue74_pkgconfig_tokens([
		'--libs',
		'mixed-case-dynamic-sentinel-74',
		'mixed-case-static-74',
	]) == ['-Wl,--issue74-dynamic-sentinel']

	assert issue74_pkgconfig_tokens(['--static', '--cflags', 'mixed-case-static-74']) == [
		'-DISSUE74_MIXED_PUBLIC',
		'-DISSUE74_MIXED_PRIVATE',
	]
	assert issue74_pkgconfig_tokens(['--static', '--libs', 'mixed-case-static-74']) == [
		'-Wl,--issue74-mixed-public',
		'-Wl,--issue74-mixed-private',
	]
}

fn test_pkgconfig_static_preserves_gnu_ld_group_alias_repetition() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'group-alias-74'
		libs:    ['-Wl,-(', '-lissue74_alias_a', '-lissue74_alias_b', '-lissue74_alias_a', '-Wl,-)']
		cflags:  []
	})
}

fn test_pkgconfig_static_matches_mergeback_around_linker_state_controls() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'linker-state-epochs-74'
		libs:    ['-Wl,--as-needed', '-lissue74_state', '-Wl,--no-as-needed', '-lissue74_state',
			'-lissue74_state']
		cflags:  []
	})
}

fn test_pkgconfig_static_preserves_repeated_whole_archive_controls() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'repeated-whole-archive-74'
		libs:    ['-Wl,--whole-archive', '-lissue74_whole_a', '-Wl,--no-whole-archive',
			'-Wl,--whole-archive', '-lissue74_whole_b', '-Wl,--no-whole-archive']
		cflags:  []
	})
}

fn test_pkgconfig_static_tracks_xlinker_whole_archive_state() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'xlinker-whole-archive-74',
	]).link_flags == [
		'-Xlinker',
		'--whole-archive',
		'-lissue74_xlinker_whole',
		'-Xlinker',
		'--no-whole-archive',
		'-lissue74_xlinker_whole',
	]
}

fn test_pkgconfig_static_does_not_broaden_xlinker_state_controls() {
	assert issue74_pkgconfig_result([
		'--static',
		'--libs',
		'xlinker-other-state-74',
	]).link_flags == [
		'-Xlinker',
		'--as-needed',
		'-Xlinker',
		'--no-as-needed',
		'-lissue74_xlinker_other',
	]
}

fn test_pkgconfig_static_preserves_libraries_across_whole_archive_epochs() {
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'whole-archive-normal-74',
	]) == ['-lissue74_epoch', '-lissue74_epoch_dependency']
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'whole-archive-normal-74',
		'whole-archive-epoch-74',
	]) == ['-lissue74_epoch', '-lissue74_epoch_dependency', '-Wl,--whole-archive', '-lissue74_epoch',
		'-Wl,--no-whole-archive']
}

fn test_pkgconfig_static_preserves_literal_archives_across_whole_archive_epochs() {
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'whole-archive-literal-normal-74',
	]) == ['/issue74/libliteral-epoch.a', '/issue74/libliteral-dependency.a']
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'whole-archive-literal-normal-74',
		'whole-archive-literal-epoch-74',
	]) == ['/issue74/libliteral-epoch.a', '/issue74/libliteral-dependency.a', '-Wl,--whole-archive',
		'/issue74/libliteral-epoch.a', '-Wl,--no-whole-archive']
}

fn test_pkgconfig_static_treats_whole_archive_start_as_idempotent() {
	assert issue74_pkgconfig_tokens([
		'--static',
		'--libs',
		'whole-archive-idempotent-74',
	]) == ['-Wl,--whole-archive', '-Wl,--whole-archive', '/issue74/libliteral-active.a',
		'-Wl,--no-whole-archive', '/issue74/libliteral-tail.a']
}

fn test_pkgconfig_dynamic_isolated_from_whole_archive_merging() {
	assert issue74_pkgconfig_tokens(['--libs', 'whole-archive-idempotent-74']) == [
		'-Wl,--whole-archive',
		'-Wl,--whole-archive',
		'/issue74/libliteral-active.a',
		'-Wl,--no-whole-archive',
		'/issue74/libliteral-tail.a',
		'/issue74/libliteral-tail.a',
	]
}

fn test_pkgconfig_static_preserves_split_linker_control_operands() {
	for case in issue74_split_linker_cases() {
		assert issue74_pkgconfig_tokens(['--static', '--libs', case.package_a, case.package_b]) == [
			case.control,
			case.operand_a,
			case.control,
			case.operand_b,
		]
	}
}

fn test_pkgconfig_dynamic_keeps_historical_split_linker_duplicate_behavior() {
	for case in issue74_split_linker_cases() {
		assert issue74_pkgconfig_tokens(['--libs', case.package_a, case.package_b]) == [
			case.control,
			case.operand_a,
			case.operand_b,
		]
	}
}

fn test_pkgconfig_static_load_propagates_link_mode_to_recursive_dependencies() {
	dynamic_pc := pkgconfig.load('recursive-static-load-root-74', pkgconfig.Options{
		path:              issue74_fixture_dir
		use_default_paths: false
	}) or { panic(err) }
	assert dynamic_pc.libs == ['-lissue74_recursive_static_root']
	assert dynamic_pc.libs_private == []
	assert dynamic_pc.cflags == ['-DISSUE74_RECURSIVE_STATIC_ROOT']
	assert dynamic_pc.cflags_private == []

	pc := pkgconfig.load('recursive-static-load-root-74', pkgconfig.Options{
		path:              issue74_fixture_dir
		use_default_paths: false
		link_mode:         .static_
	}) or { panic(err) }
	assert pc.libs == ['-lissue74_recursive_static_root', '-Wl,--issue74-mixed-public']
	assert pc.libs_private == ['-Wl,--issue74-mixed-private']
	assert pc.cflags == ['-DISSUE74_RECURSIVE_STATIC_ROOT', '-DISSUE74_MIXED_PUBLIC']
	assert pc.cflags_private == ['-DISSUE74_MIXED_PRIVATE']
}

fn test_pkgconfig_static_trims_field_names_without_changing_dynamic_parser() {
	assert issue74_pkgconfig_tokens(['--libs', 'spaced-fields-74']) == []
	assert issue74_pkgconfig_tokens(['--cflags', 'spaced-fields-74']) == []
	issue74_assert_static_case(Issue74StaticCase{
		package: 'spaced-fields-74'
		libs:    ['-lissue74_spaced_root', '-lissue74_spaced_root_private', '-lissue74_private',
			'-lissue74_private_private']
		cflags:  ['-DISSUE74_SPACED_ROOT', '-DISSUE74_PRIVATE', '-DISSUE74_SPACED_ROOT_PRIVATE',
			'-DISSUE74_PRIVATE_PRIVATE']
	})
}

fn test_pkgconfig_static_shared_public_private_dependency_is_emitted_once() {
	issue74_assert_static_case(Issue74StaticCase{
		package: 'shared-public-private-edge-74'
		libs:    ['-lissue74_shared_edge_root', '-lissue74_public', '-lissue74_public_private']
		cflags:  ['-DISSUE74_SHARED_EDGE_ROOT', '-DISSUE74_PUBLIC', '-DISSUE74_PUBLIC_PRIVATE']
	})
}
