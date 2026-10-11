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
			dir := os.join_path(root_dir(vroot), '${compiler}_${prod}')
			os.mkdir_all(dir)!
			record(dir, vroot, flags, false, sources)!
			// Recording the same build twice must not duplicate or lose modules.
			record(dir, vroot, flags, false, sources)!
			for name in ['builtin', 'crypto.sample'] {
				expected << Request{ args: flags, module: name }
			}
		}
	}
	requests := collect(vroot)!
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
		dir := os.join_path(root_dir(vroot), config)
		os.mkdir_all(dir)!
		os.write_file(os.join_path(dir, 'sample_${hash_text(os.real_path(os.dir(source)))}.vh'), '')!
		// A module recorded in another configuration must not hide this old one.
		if config == 'new' {
			record(dir, vroot, ['-cc', 'custom-cc'], false, {
				'sample': [source]
			})!
		}
	}
	requests := collect(vroot)!
	assert Request{ args: ['-cc', 'custom-cc'], module: 'crypto.sample' } in requests
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
		args:    ['-cc', 'cc', '-cflags', '-DNAME="value with spaces"\n-DSECOND', '', '-prod']
		module:  'crypto.sha3'
		is_test: true
	}
	assert decode_request(encode_request(request))! == request
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

fn test_refresh_rebuilds_changed_vlib_dependencies_for_each_compiler_and_mode() ! {
	$if windows {
		return
	}
	vroot := os.dir(@VEXE)
	root := os.join_path(os.vtmp_dir(), 'vup_cache_integration_${os.getpid()}')
	os.mkdir_all(root)!
	module_root := os.join_path(vroot, 'vlib', 'v', 'util', 'cache_refresh', 'testdata',
		'refresh_${os.getpid()}')
	import_root := 'v.util.cache_refresh.testdata.refresh_${os.getpid()}'
	dependency := os.join_path(module_root, 'dependency', 'dependency.v')
	probe := os.join_path(module_root, 'probe', 'probe.v')
	for file in [dependency, probe] { os.mkdir_all(os.dir(file))! }
	os.write_file(dependency, 'module dependency\npub fn value() int { return 40 }\n')!
	os.write_file(probe, 'module probe\nimport ${import_root}.dependency\n' +
		'pub fn value() int {\n\t\$if prod { return dependency.value() + 2 }\n' +
		'\treturn dependency.value()\n}\n')!
	saved := os.environ()
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	os.unsetenv('VFLAGS')
	os.unsetenv('CFLAGS')
	os.unsetenv('LDFLAGS')
	os.unsetenv('V3_CACHE_FORCE_SOURCE')
	os.unsetenv('V3_CACHE_TRACE')
	defer {
		for name in ['V3CACHE', 'VEXE', 'V_SKIP_VVMRC', 'VFLAGS', 'CFLAGS', 'LDFLAGS',
			'V3_CACHE_FORCE_SOURCE', 'V3_CACHE_TRACE'] {
			if name in saved { os.setenv(name, saved[name], true) } else { os.unsetenv(name) }
		}
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'import ${import_root}.probe\nfn main() { println(probe.value()) }\n')!
	mut builds := [][]string{}
	for compiler in ['cc', 'clang', 'gcc', 'tcc'] {
		if compiler == 'tcc' {
			if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) { continue }
		} else {
			os.find_abs_path_of_executable(compiler) or { continue }
		}
		for prod in [false, true] {
			mut flags := ['-cc', compiler, '-gc', 'none', '-usecache']
			if prod { flags << '-prod' }
			mut process := os.new_process(@VEXE)
			process.set_args([...flags, '-o', os.join_path(root, 'program'), main_file])
			process.set_redirect_stdio()
			process.run()
			stdout := process.stdout_slurp()
			stderr := process.stderr_slurp()
			process.wait()
			assert process.code == 0, '${flags}: ${stdout}\n${stderr}'
			process.close()
			assert stdout.contains('Caching module ${import_root}.probe...'), '${flags}: ${stdout}\n${stderr}'
			assert !stderr.contains('Caching module '), stderr
			builds << flags
		}
	}
	requests := collect(vroot)!.filter(it.module.starts_with(import_root + '.'))
	assert requests.len == builds.len * 2, requests.str()
	// Change only an imported vlib dependency. Both it and its dependent object
	// must be refreshed; the original programs then use them without recaching.
	os.write_file(dependency, 'module dependency\npub fn value() int { return 43 }\n')!
	refresh(@VEXE, requests)!
	for flags in builds {
		result := os.exec([@VEXE, ...flags, '-o', os.join_path(root, 'program'), main_file])
		assert result.exit_code == 0, result.output
		assert !result.output.contains('Caching module ${import_root}.'), result.output
		run := os.exec([os.join_path(root, 'program')])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == if '-prod' in flags { '45' } else { '43' }, run.output
	}
}
