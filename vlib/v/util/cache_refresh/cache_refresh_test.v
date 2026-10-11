module cache_refresh

import os

fn test_inventory_preserves_compiler_modes_and_flags_and_excludes_project_modules() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_inventory_${os.getpid()}')
	os.mkdir_all(root)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'crypto', 'sample', 'sample.v')
	builtin := os.join_path(vroot, 'vlib', 'builtin', 'builtin.v')
	local := os.join_path(root, 'project', 'local.v')
	for file in [source, builtin, local] {
		os.mkdir_all(os.dir(file))!
		os.write_file(file, 'module sample\n')!
	}
	sources := {
		'sample':  [source]
		'builtin': [builtin]
		'local':   [local]
	}
	mut expected := []Request{}
	for compiler in ['tcc', 'cc', 'clang', 'gcc'] {
		for prod in [false, true] {
			mut flags := ['-cc', compiler, '-cflags', '-DSAMPLE="value with spaces"']
			if prod { flags << '-prod' }
			dir := os.join_path(root_dir(vroot, @VEXE), '${compiler}_${prod}')
			os.mkdir_all(dir)!
			record(dir, vroot, flags, false, sources, ['-DSAMPLE=value with spaces'])!
			// Recording the same build twice must not duplicate or lose modules.
			record(dir, vroot, flags, false, sources, ['-DSAMPLE=value with spaces'])!
			for name in ['builtin', 'crypto.sample'] {
				expected << Request{
					args:             flags
					module:           name
					cwd:              os.getwd()
					object_flags:     ['-DSAMPLE=value with spaces']
					has_object_flags: true
				}
			}
		}
	}
	requests := collect(vroot, @VEXE)!
	assert requests.len == expected.len
	for request in expected {
		assert request in requests, request.str()
	}
	assert !requests.any(it.module == 'local')
}

fn test_inventory_handles_legacy_headers_and_partly_recorded_configurations() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_legacy_inventory_${os.getpid()}')
	os.mkdir_all(root)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	vroot := os.join_path(root, 'checkout')
	source := os.join_path(vroot, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	for config in ['old', 'new'] {
		dir := os.join_path(root_dir(vroot, @VEXE), config)
		os.mkdir_all(dir)!
		os.write_file(os.join_path(dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
		// A module recorded in another configuration must not hide this old one.
		if config == 'new' {
			record(dir, vroot, ['-cc', 'custom-cc'], false, {
				'sample': [source]
			}, [])!
		}
	}
	requests := collect(vroot, @VEXE)!
	assert Request{ args: ['-cc', 'custom-cc'], module: 'crypto.sample', cwd: os.getwd(), has_object_flags: true } in requests
	if _ := os.find_abs_path_of_executable('cc') {
		assert Request{ args: ['-cc', 'cc'], module: 'crypto.sample' } in requests
		assert Request{ args: ['-cc', 'cc', '-prod'], module: 'crypto.sample' } in requests
	}
	assert requests.all(it.module == 'crypto.sample')
}

fn test_module_names_do_not_allow_source_injection() {
	assert valid_module('crypto.sha3')
	assert valid_module('builtin.closure')
	for invalid in ['', '.', 'a..b', '../local', 'sample\nfn main() {}', 'sample as _'] {
		assert !valid_module(invalid), invalid
	}
}

fn test_inventory_argument_encoding_preserves_newlines_and_empty_values() ! {
	request := Request{
		args:             ['-cc', 'cc', '-cflags', '-DNAME="value with spaces"\n-DSECOND', '',
			'-prod']
		module:           'crypto.sha3'
		is_test:          true
		cwd:              os.join_path(os.getwd(), 'directory with spaces\nand newlines')
		object_flags:     ['-DNAME=value with spaces\nand newlines', '-I', 'include path', '']
		has_object_flags: true
	}
	assert decode_request(encode_request(request))! == request
	assert decode_request('${cwd_record_format}\ncrypto.sha3\nprogram\n0\n3\n-cc2\ncc')! == Request{
		args:   ['-cc', 'cc']
		module: 'crypto.sha3'
	}
	assert decode_request('${legacy_record_format}\ncrypto.sha3\nprogram\n3\n-cc2\ncc')! == Request{
		args:   [
			'-cc',
			'cc',
		]
		module: 'crypto.sha3'
	}
	for content in ['', 'old-format\ncrypto.sha3\nprogram\n',
		'${record_format}\ncrypto.sha3\nprogram\n100\nshort'] {
		if _ := decode_request(content) {
			assert false, content
		}
	}
}

fn test_refresh_skips_removed_modules_without_starting_the_compiler() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_removed_module_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib'))!
	defer { os.rmdir_all(root) or {} }
	// There is no compiler here: attempting to compile the retired import fails.
	refresh(os.join_path(root, 'v'), [Request{ args: ['-cc', 'cc'], module: 'retired' }])!
}

fn test_inventory_and_refresh_skip_vanished_working_directories() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_vanished_directory_${os.getpid()}')
	source := os.join_path(root, 'vlib', 'sample', 'sample.v')
	project := os.join_path(root, 'project')
	previous_cache := os.getenv_opt('V3CACHE')
	previous_dir := os.getwd()
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(project)!
	os.write_file(source, 'module sample\n')!
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		os.chdir(previous_dir) or {}
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root_dir(root, @VEXE), 'config')
	os.mkdir_all(dir)!
	// A stale record still owns its header; it must not fall back to legacy refreshes.
	os.write_file(os.join_path(dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
	os.chdir(project)!
	record(dir, root, ['-cflags', '-include config.h -I include'], false, {
		'sample': [source]
	}, [])!
	os.chdir(previous_dir)!
	requests := collect(root, @VEXE)!
	assert requests.len == 1 && requests[0].cwd == project
	os.mv(project, '${project}_moved')!
	assert collect(root, @VEXE)!.len == 0
	// There is no compiler: a failed child chdir or attempted compile would fail.
	refresh(os.join_path(root, 'v'), requests)!
	os.rmdir_all('${project}_moved')!
	refresh(os.join_path(root, 'v'), requests)!
	// Live records in the same configuration remain refreshable.
	record(dir, root, ['-cc', 'cc'], false, {
		'sample': [source]
	}, [])!
	assert collect(root, @VEXE)! == [Request{
		args:             ['-cc', 'cc']
		module:           'sample'
		cwd:              previous_dir
		has_object_flags: true
	}]
}

fn test_refresh_uses_recorded_directory_for_relative_compile_inputs() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_relative_inputs_${os.getpid()}')
	project := os.join_path(root, 'project with spaces')
	os.mkdir_all(os.join_path(project, 'include path'))!
	os.write_file(os.join_path(project, 'config.h'), '#include <refresh_input.h>\n')!
	os.write_file(os.join_path(project, 'include path', 'refresh_input.h'), '#define REFRESH_INPUT 1\n')!
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	previous_dir := os.getwd()
	defer {
		os.chdir(previous_dir) or {}
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, 'config')
	os.mkdir_all(dir)!
	os.chdir(project)!
	record(dir, root, ['-cflags', '-include config.h -I "include path"'], false, {
		'sample': [source]
	}, ['-include', 'config.h', '-I', 'include path'])!
	entries := os.ls(dir)!.filter(it.starts_with(record_prefix))
	assert entries.len == 1
	request := decode_request(os.read_file(os.join_path(dir, entries[0]))!)!
	assert request.cwd == project
	compiler := os.join_path(root, 'v')
	log := os.join_path(root, 'cwd.log')
	os.write_file(compiler, '#!/bin/sh\n' +
		'test "\$1" = "-cflags" || exit 2\n' +
		'test "\$2" = \'-include config.h -I "include path"\' || exit 3\n' +
		'test -f config.h && test -f "include path/refresh_input.h" || exit 4\n' +
		'test "\$V3_CACHE_REFRESH_OBJECT_FLAGS" = \'"-include" "config.h" "-I" "include path"\' || exit 5\n' +
		'printf "%s" "\$PWD" > ' + os.quoted_path(log) + '\n')!
	os.chmod(compiler, 0o755)!
	os.chdir(root)!
	refresh(compiler, [request])!
	assert os.read_file(log)! == project
	assert os.getwd() == root
}

fn test_refresh_rebuilds_changed_vlib_dependencies_for_each_compiler_and_mode() ! {
	$if windows {
		return
	}
	vroot := os.dir(@VEXE)
	root := os.join_path(os.vtmp_dir(), 'vup_cache_integration_${os.getpid()}')
	os.mkdir_all(root)!
	project := os.join_path(root, 'project with spaces')
	os.mkdir_all(os.join_path(project, 'modules'))!
	library_dir := os.join_path(root, 'libs')
	os.mkdir_all(library_dir)!
	c_source := os.join_path(library_dir, 'answer.c')
	c_object := os.join_path(library_dir, 'answer.o')
	os.write_file(c_source, 'int answer(void) { return 42; }\n')!
	for command in [['cc', '-c', c_source, '-o', c_object],
		['ar', 'rcs', os.join_path(library_dir, 'libanswer.a'), c_object]] {
		result := os.exec(command)
		assert result.exit_code == 0, result.output
	}
	module_root := os.join_path(vroot, 'vlib', 'v', 'util', 'cache_refresh', 'testdata',
		'refresh_${os.getpid()}')
	import_root := 'v.util.cache_refresh.testdata.refresh_${os.getpid()}'
	dependency := os.join_path(module_root, 'dependency', 'dependency.v')
	probe := os.join_path(module_root, 'probe', 'probe.v')
	for file in [dependency, probe] { os.mkdir_all(os.dir(file))! }
	// Native search paths inside vlib support module caching. A relative link
	// reaches this directory from the project, but not from the updater's cwd.
	os.mkdir_all(os.join_path(module_root, 'include'))!
	os.symlink(os.join_path(module_root, 'include'), os.join_path(root, 'include path'))!
	os.symlink(os.join_path(module_root, 'include'), os.join_path(root, 'include'))!
	os.write_file(dependency, 'module dependency\n#flag -DREFRESH_DEPENDENCY\npub fn value() int { return 40 }\n')!
	os.write_file(probe, 'module probe\nimport ${import_root}.dependency\n' +
		'pub fn value() int {\n\t\$if prod { return dependency.value() + 2 }\n' +
		'\treturn dependency.value()\n}\n')!
	saved := os.environ()
	os.unsetenv('V3CACHE')
	os.setenv('VTMP', os.join_path(root, 'cache'), true)
	os.setenv('V3_TEST_ISOLATE_CACHE', '1', true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	os.unsetenv('VFLAGS')
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.unsetenv('V3_CACHE_TRACE')
	os.unsetenv('V3_CACHE_REFRESH')
	os.unsetenv('VTEST_ONLY_FN')
	previous_dir := os.getwd()
	defer {
		for name in ['V3CACHE', 'VTMP', 'V3_TEST_ISOLATE_CACHE', 'VEXE', 'V_SKIP_VVMRC', 'VFLAGS',
			'CFLAGS', 'LDFLAGS', 'PATH', 'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE', 'V3_CACHE_REFRESH',
			'VTEST_ONLY_FN'] {
			if name in saved { os.setenv(name, saved[name], true) } else { os.unsetenv(name) }
		}
		os.chdir(previous_dir) or {}
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(project, 'main.v')
	forced_header := os.join_path(module_root, 'include', 'config.h')
	os.write_file(forced_header, '#define REFRESH_SOURCE_INPUT 1\n')!
	source_flags := '#flag -DREFRESH_PROJECT\n#flag -include "${forced_header}"\n'
	os.write_file(main_file, source_flags + 'import ${import_root}.probe\nfn main() { println(probe.value()) }\n')!
	mut supported_modes := ['-no-std', '-strict']
	$if linux {
		libc := os.exec(['ldd', '--version'])
		supported_modes << if libc.output.contains('musl') { '-musl' } else { '-glibc' }
	}
	os.chdir(project)!
	mut builds := [][]string{}
	for compiler in ['cc', 'clang', 'gcc', 'tcc'] {
		if compiler == 'tcc' {
			if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) { continue }
		} else {
			os.find_abs_path_of_executable(compiler) or { continue }
		}
		mut modes := [][]string{len: 1}
		if compiler == 'cc' {
			for mode in supported_modes { modes << [mode] }
		}
		for mode in modes {
			for prod in [false, true] {
				mut flags := ['-cc', compiler, '-gc', 'none', '-usecache', '-path',
					'modules/../modules|@vlib']
				if prod { flags << '-prod' }
				flags << mode
				if compiler == 'cc' && !prod { flags << ['-ldflags', '-L../libs -lanswer'] }
				if compiler == 'cc' {
					flags << ['-cflags', '-L../libs -lanswer -DREFRESH_LABEL="a b"']
				}
				flags << ['-cflags', '-I "../include path"']
				if compiler == 'gcc' && prod { flags << '-silent' }
				mut process := os.new_process(@VEXE)
				process.set_args([...flags, '-o', os.join_path(root, 'program'), 'run', main_file])
				process.set_redirect_stdio()
				process.run()
				stdout := process.stdout_slurp()
				stderr := process.stderr_slurp()
				process.wait()
				assert process.code == 0, '${flags}: ${stdout}\n${stderr}'
				process.close()
				assert stdout.trim_space() == if prod { '42' } else { '40' }, '${flags}: ${stdout}\n${stderr}'
				if '-silent' in flags {
					assert !stderr.contains('Caching module '), stderr
				} else if builds.len == 0 {
					assert stderr.contains('Caching module ${import_root}.probe...'), stderr
				}
				builds << flags
			}
		}
	}
	// Cover both direct selectors and vtest's transient VTEST_ONLY_FN transport.
	test_file := os.join_path(project, 'selected_test.v')
	os.write_file(test_file, source_flags + 'import ${import_root}.probe\n' +
		'fn test_selected_one() { assert probe.value() > 0 }\n' +
		'fn test_selected_two() { assert probe.value() > 0 }\n' +
		'fn test_unselected() { panic("unselected test ran") }\n')!
	mut test_builds := [][]string{}
	for through_vtest in [false, true] {
		if through_vtest {
			// vtest replaces VTMP with a disposable session directory. Pin its
			// cache to the existing isolated compiler cache instead.
			os.setenv('V3CACHE', os.dir(root_dir(vroot, @VEXE)), true)
		}
		selector := if through_vtest {
			'test_selected_one, test_selected_two'
		} else {
			'test_selected_*'
		}
		for prod in [false, true] {
			mut flags := ['-cc', 'cc', '-gc', 'none', '-usecache', '-path', 'modules/../modules|@vlib',
				'-cflags', '-I../include']
			if prod { flags << '-prod' }
			command := if through_vtest {
				[@VEXE, ...flags, 'test', '-run-only', selector, test_file]
			} else {
				[@VEXE, ...flags, '-run-only', selector, '-o', os.join_path(root, 'test_program'),
					test_file]
			}
			result := os.exec(command)
			assert result.exit_code == 0, result.output
			flags << ['-run-only', selector]
			test_builds << flags
		}
		os.unsetenv('V3CACHE')
	}
	requests := collect(vroot, @VEXE)!.filter(it.module.starts_with(import_root + '.'))
	assert requests.len > 0
	assert collect(vroot, os.executable())!.filter(it.module.starts_with(import_root + '.')).len == 0
	// Compiler aliases can share a configuration and restore its program before
	// reaching the inventory writer; its existing record already covers the cache.
	assert requests.len >= 2 && requests.len <= (builds.len + test_builds.len) * 2, requests.str()
	for flags in test_builds {
		selector := flags[flags.index('-run-only') + 1]
		assert requests.any(it.is_test && selector in it.args && ('-prod' in flags) == ('-prod' in it.args)), flags.str()
	}
	for mode in supported_modes {
		assert requests.any(mode in it.args), mode
	}
	for request in requests {
		assert request.has_object_flags, request.str()
		assert '-DREFRESH_PROJECT' in request.object_flags, request.str()
		assert '-DREFRESH_DEPENDENCY' in request.object_flags, request.str()
		assert forced_header in request.object_flags, request.str()
		assert !request.object_flags.any(it.contains('../libs') || it.contains('-lanswer')), request.str()
		assert request.cwd == project, request.str()
		assert '-ldflags' !in request.args, request.str()
		path_index := request.args.index('-path')
		assert path_index >= 0, request.str()
		assert request.args[path_index + 1] == [os.join_path(project, 'modules'),
			os.join_path(vroot, 'vlib')].join('|'), request.str()
		cc_index := request.args.index('-cc')
		assert cc_index >= 0 && os.is_abs_path(request.args[cc_index + 1]), request.str()
		assert !request.args.any(it.contains('../libs') || it.contains('-lanswer')), request.str()
	}
	// Run the refresh in another directory and capture its streams separately.
	// Its importers must warm the same absolute-path cache configuration, and
	// cannot link against this project's output-relative library.
	os.chdir(previous_dir)!
	refresh_source := os.join_path(root, 'refresh_helper.v')
	refresh_exe := os.join_path(root, 'refresh_helper')
	os.write_file(refresh_source, 'import os\nimport v.util.cache_refresh\n' +
		'fn main() {\n' +
		'\trequests := cache_refresh.collect(os.dir(@VEXE), @VEXE)!.filter(it.module.starts_with("${import_root}."))\n' +
		'\tcache_refresh.refresh(@VEXE, requests)!\n}\n')!
	helper := os.exec([@VEXE, '-cc', 'cc', '-gc', 'none', '-nocache', '-o', refresh_exe,
		refresh_source])
	assert helper.exit_code == 0, helper.output
	// Change PATH after creating the inventory: compiler names now point at
	// unusable stand-ins, while the recorded absolute commands still work.
	changed_path := os.join_path(root, 'changed_path')
	os.mkdir_all(changed_path)!
	for name in ['cc', 'clang', 'gcc'] {
		path := os.join_path(changed_path, name)
		os.write_file(path, '#!/bin/sh\necho wrong compiler >&2\nexit 1\n')!
		os.chmod(path, 0o755)!
	}
	os.setenv('PATH', changed_path + os.path_delimiter + saved['PATH'], true)
	os.setenv('VTEST_ONLY_FN', 'ambient_selector_must_not_override_inventory', true)
	// Change only an imported vlib dependency. Refresh its implementation while
	// reusing the unchanged dependent object; original programs must not recache.
	os.write_file(dependency, 'module dependency\n#flag -DREFRESH_DEPENDENCY\npub fn value() int { return 43 }\n')!
	mut refresher := os.new_process(refresh_exe)
	refresher.set_redirect_stdio()
	refresher.run()
	stdout := refresher.stdout_slurp()
	stderr := refresher.stderr_slurp()
	refresher.wait()
	assert refresher.code == 0, '${stdout}\n${stderr}'
	refresher.close()
	assert stdout.contains('Caching module ${import_root}.dependency...'), stdout
	assert !stderr.contains('Caching module '), stderr
	os.setenv('PATH', saved['PATH'], true)
	os.unsetenv('VTEST_ONLY_FN')
	os.chdir(project)!
	for flags in builds {
		result := os.exec([@VEXE, ...flags, '-o', os.join_path(root, 'program'), main_file])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('Caching module ${import_root}.'), result.output
		run := os.exec([os.join_path(root, 'program')])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == if '-prod' in flags { '45' } else { '43' }, run.output
	}
	for flags in test_builds {
		selector := flags.index('-run-only')
		os.setenv('VTEST_ONLY_FN', flags[selector + 1], true)
		result := os.exec([@VEXE, ...flags[..selector], '-skip-running', '-o',
			os.join_path(root, 'test_program'), test_file])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('Caching module ${import_root}.'), result.output
	}
}
