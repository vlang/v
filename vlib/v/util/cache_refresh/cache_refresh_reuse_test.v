module cache_refresh

import os

fn test_compiler_updates_reuse_unchanged_generated_module_objects() ! {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'vup_compiler_object_reuse_${os.getpid()}')
	os.mkdir_all(root)!
	compilers := [0, 1].map(os.join_path(os.dir(@VEXE), 'vup_object_reuse_${os.getpid()}_${it}'))
	defer {
		for compiler in compilers { os.rm(compiler) or {} }
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import crypto.sha3 as _\nfn main() {}\n')!
	mut environment := os.environ()
	for name in ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_FORCE_SOURCE'] {
		environment.delete(name)
	}
	environment['V3CACHE'] = os.join_path(root, 'cache')
	environment['V3_CACHE_REFRESH'] = '1'
	environment['V_SKIP_VVMRC'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	for index in 0 .. 2 {
		// Different executable identities force fresh compiler-specific interfaces
		// even though these compilers generate the same module C.
		compiler := compilers[index]
		os.cp(@VEXE, compiler)!
		os.chmod(compiler, 0o755)!
		environment['VEXE'] = compiler
		mut process := os.new_process(compiler)
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.set_args(['-cc', 'clang', '-gc', 'none', '-skip-running', '-o',
			os.join_path(root, 'program'), source])
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert code == 0, output
		if index == 0 {
			assert output.contains('Caching module crypto.sha3...'), output
		} else {
			assert !output.contains('Caching module '), output
		}
	}
}

fn test_refresh_stops_on_disk_full_and_streams_large_child_output() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_disk_full_${os.getpid()}')
	for name in ['first', 'second'] { os.mkdir_all(os.join_path(root, 'vlib', name))! }
	defer { os.rmdir_all(root) or {} }
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\n' +
		'echo called >> ' + os.quoted_path(log) + '\n' +
		'awk \'BEGIN { for (i = 0; i < 20000; i++) print "cache output" }\'\n' +
		'printf "%s" "No space left " >&2\nsleep 0.02\n' +
		'printf "%s\\n" "on device" >&2\nexit 7\n')!
	os.chmod(vexe, 0o755)!
	source := os.join_path(root, 'helper.v')
	os.write_file(source, 'import v.util.cache_refresh\nfn main() {\n' +
		'cache_refresh.refresh("${vexe}", [cache_refresh.Request{module:"first"}, cache_refresh.Request{module:"second"}])!\n}\n')!
	helper := os.join_path(root, 'helper')
	build := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', helper, source])
	assert build.exit_code == 0, build.output
	stdout := os.join_path(root, 'stdout')
	stderr := os.join_path(root, 'stderr')
	result := os.exec(['sh', '-c',
		'${os.quoted_path(helper)} > ${os.quoted_path(stdout)} 2> ${os.quoted_path(stderr)}'])
	output := os.read_file(stdout)! + os.read_file(stderr)!
	assert result.exit_code != 0, output
	assert output.count('cache output') == 20000
	assert output.contains('No space left on device'), output
	assert output.contains('disk is full; cache refresh stopped'), output
	assert output.contains('exit 7'), output
	assert !output.contains('> Module cache refresh complete:'), output
	assert os.read_file(log)!.split_into_lines().len == 1
}

fn test_synthetic_refresh_reports_failed_tinycc_module_objects() ! {
	$if !linux && !macos {
		return
	}
	vroot := os.dir(@VEXE)
	if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'vup_failed_tcc_object_${os.getpid()}')
	name := 'vup_failed_tcc_object_${os.getpid()}'
	module_root := os.join_path(vroot, 'vlib', name)
	os.mkdir_all(module_root)!
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	// This unused public function is compiled into the module object, but a
	// whole-program fallback would prune it and hide the missing C definition.
	os.write_file(os.join_path(module_root, 'value.c.v'), 'module ${name}\n' +
		'pub fn value() int { return int(C.VUP_REFRESH_MISSING_DEFINE) }\n')!
	source := os.join_path(root, 'helper.v')
	os.write_file(source, 'import v.util.cache_refresh\nfn main() {\n' +
		'cache_refresh.refresh(@VEXE, [cache_refresh.Request{module: "${name}", args: ["-cc", "tcc"]}])!\n}\n')!
	helper := os.join_path(root, 'helper')
	build := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', helper, source])
	assert build.exit_code == 0, build.output
	mut environment := os.environ()
	for variable in ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH', 'V3_CACHE_FORCE_SOURCE'] {
		environment.delete(variable)
	}
	environment['V3CACHE'] = os.join_path(root, 'cache')
	environment['VEXE'] = @VEXE
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	mut process := os.new_process(helper)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	code := process.code
	process.close()
	assert code != 0, output
	assert output.contains('failed to build cached module object'), output
	assert output.contains('VUP_REFRESH_MISSING_DEFINE'), output
	assert !output.contains('> Module cache refresh complete:'), output
	// An ordinary build still uses TinyCC's source fallback successfully.
	entry := os.join_path(root, 'main.v')
	os.write_file(entry, 'import ${name} as _\nfn main() {}\n')!
	program := os.join_path(root, 'program')
	mut ordinary := os.new_process(@VEXE)
	ordinary.set_environment(environment)
	ordinary.set_redirect_stdio()
	ordinary.set_args(['-cc', 'tcc', '-o', program, entry])
	ordinary.wait()
	ordinary_output := ordinary.stdout_slurp() + ordinary.stderr_slurp()
	ordinary_code := ordinary.code
	ordinary.close()
	assert ordinary_code == 0 && os.is_file(program), ordinary_output
	ran := os.exec([program])
	assert ran.exit_code == 0 && ran.output == '', ran.output
}

fn test_synthetic_refresh_only_compiles_module_objects_on_cold_and_warm_caches() ! {
	$if windows {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'vup_objects_only_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.environ()
	variables := ['V3CACHE', 'VEXE', 'VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH',
		'V3_CACHE_REFRESH_ENTRY', 'V3_CACHE_REFRESH_PROJECT_FLAGS', 'V3_CACHE_FORCE_SOURCE',
		'V_SKIP_VVMRC', 'V_MACOS_V3_NO_FALLBACK']
	defer {
		for variable in variables {
			if variable in saved {
				os.setenv(variable, saved[variable], true)
			} else {
				os.unsetenv(variable)
			}
		}
		os.rmdir_all(root) or {}
	}
	for variable in variables { os.unsetenv(variable) }
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	log := os.join_path(root, 'calls')
	dependency_log := os.join_path(root, 'dependency_calls')
	compiler := os.join_path(root, 'cc')
	// Module compilation is allowed. Compiling a program unit or linking the
	// synthetic importer must fail, even if the ordinary compiler could do it.
	// Record native helper dependency scans separately so warm refresh still
	// forbids every new object while allowing dependency validation.
	os.write_file(compiler, '#!/bin/sh\n' +
		'case "\$*" in --version) exec ' + os.quoted_path(cc) + ' "\$@";; esac\n' +
		'case "\$*" in *program_main_*|*program_prefix_*) echo "unexpected program object" >&2; exit 97;; esac\n' +
		'for arg; do\n  if [ "\$arg" = -M ]; then printf "%s\\n" "\$*" >> ' +
		os.quoted_path(dependency_log) + '; exec ' + os.quoted_path(cc) + ' "\$@"; fi\ndone\n' +
		'printf "%s\\n" "\$*" >> ' + os.quoted_path(log) + '\n' +
		'for arg; do\n  if [ "\$arg" = -c ]; then exec ' + os.quoted_path(cc) + ' "\$@"; fi\ndone\n' +
		'echo "unexpected program link" >&2\nexit 98\n')!
	os.chmod(compiler, 0o755)!
	for is_test in [false, true] {
		// Overlapping import sets share compiler flags but can select different
		// interface implementations. Every variant must survive a complete pass.
		requests := [Request{
			args:    ['-cc', compiler]
			module:  'math.big'
			imports: ['crypto.ed25519', 'math.big']
			is_test: is_test
		}, Request{
			args:    ['-cc', compiler]
			module:  'os'
			imports: ['os', 'flag', 'time']
			is_test: is_test
		}, Request{
			args:    ['-cc', compiler]
			module:  'term'
			imports: ['term', 'time']
			is_test: is_test
		}, Request{
			args:    ['-cc', compiler]
			module:  'runtime'
			imports: ['runtime', 'sync', 'time']
			is_test: is_test
		}]
		refresh(@VEXE, requests)!
		cold_calls := os.read_file(log)!
		assert cold_calls.len > 0
		assert !cold_calls.contains('program_main_') && !cold_calls.contains('program_prefix_'), cold_calls
		assert collect(os.dir(@VEXE), @VEXE)!.any(it.module == 'math.big' && it.is_test == is_test)
		refresh(@VEXE, requests)!
		assert os.read_file(log)! == cold_calls, 'warm refresh must reuse its validated module objects'
	}
}

fn test_inventory_preserves_distinct_import_orders() ! {
	root := os.join_path(os.vtmp_dir(), 'vup_no_recursive_inventory_${os.getpid()}')
	source := os.join_path(root, 'vlib', 'sample', 'sample.v')
	other := os.join_path(root, 'vlib', 'other', 'other.v')
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(os.dir(other))!
	os.write_file(source, 'module sample\n')!
	os.write_file(other, 'module other\n')!
	dir := os.join_path(root, 'cache')
	os.mkdir_all(dir)!
	previous := os.getenv_opt('V3_CACHE_REFRESH')
	defer {
		if value := previous {
			os.setenv('V3_CACHE_REFRESH', value, true)
		} else {
			os.unsetenv('V3_CACHE_REFRESH')
		}
		os.rmdir_all(root) or {}
	}
	os.unsetenv('V3_CACHE_REFRESH')
	sources := {
		'sample': [source]
		'other':  [other]
	}
	record(dir, root, ['-cc', 'cc'], false, sources, [], '', false)!
	before := os.ls(dir)!
	assert before.len == 2
	os.setenv('V3_CACHE_REFRESH', '1', true)
	record(dir, root, ['-cc', 'cc'], false, sources, [], '', false)!
	assert os.ls(dir)! == before
	record(dir, root, ['-cc', 'cc'], false, {
		'other':  [other]
		'sample': [source]
	}, [], '', false)!
	assert os.ls(dir)!.len == 4
	// Order-sensitive module directives must retain separate inventory identities.
	a := Request{ module: 'sample', imports: ['sample', 'other'] }
	b := Request{ ...a, imports: ['other', 'sample'] }
	assert encode_request(a) != encode_request(b)
	assert decode_request(encode_request(a))! == a
	assert decode_request(encode_request(b))! == b
}

fn test_synthetic_refresh_replays_each_import_sequence_once() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_synthetic_reuse_${os.getpid()}')
	for name in ['builtin', 'sample', 'other'] {
		os.mkdir_all(os.join_path(root, 'vlib', name))!
	}
	defer { os.rmdir_all(root) or {} }
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\nfor source; do :; done\n' +
		'cat "$source" >> ' + os.quoted_path(log) + '\n')!
	os.chmod(vexe, 0o755)!
	requests := ['builtin', 'sample', 'other'].map(Request{
		module:  it
		imports: ['sample', 'other']
	})
	refresh(vexe, requests)!
	contents := os.read_file(log)!
	assert contents.count('fn main() {}') == 1, contents
	assert contents.count('import sample as _') == 1, contents
	assert contents.count('import other as _') == 1, contents
	assert contents.contains('import sample as _\nimport other as _\n'), contents
	// Different import orders can change the effective C flags and stay separate.
	refresh(vexe, [requests[1], Request{ ...requests[2], imports: ['other', 'sample'] }])!
	assert os.read_file(log)!.count('fn main() {}') == 3
	assert os.read_file(log)!.contains('import other as _\nimport sample as _\n')
	// Compiler modes, test mode, source flags and import sets remain separate.
	refresh(vexe, [
		Request{ ...requests[1], args: ['-prod'] },
		Request{ ...requests[1], is_test: true },
		Request{ ...requests[1], show_test_stats: true },
		Request{ ...requests[1], project_flags: [FlagGroup{ flags: ['-DOTHER'] }] },
		Request{ module: 'sample', imports: ['sample'] },
	])!
	after := os.read_file(log)!
	assert after.count('fn main() {}') == 7, after
	assert after.count('fn test_cache_refresh() {}') == 1, after
}

fn test_opaque_headers_preserve_ordered_include_paths_without_refresh() ! {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('clang') or { return }
	vroot := os.dir(@VEXE)
	root := os.join_path(os.vtmp_dir(), 'vup_ordered_flags_${os.getpid()}')
	name := 'vup_ordered_flags_${os.getpid()}'
	module_root := os.join_path(vroot, 'vlib', name)
	os.mkdir_all(root)!
	saved := os.environ()
	variables := ['V3CACHE', 'VEXE', 'VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH',
		'V3_CACHE_REFRESH_ENTRY', 'V3_CACHE_REFRESH_PROJECT_FLAGS', 'V3_CACHE_FORCE_SOURCE',
		'V_SKIP_VVMRC']
	defer {
		for variable in variables {
			if variable in saved {
				os.setenv(variable, saved[variable], true)
			} else {
				os.unsetenv(variable)
			}
		}
		os.rmdir_all(module_root) or {}
		os.rmdir_all(root) or {}
	}
	for variable in variables { os.unsetenv(variable) }
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('VEXE', @VEXE, true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	mut headers := []string{}
	for i, module_name in ['z_first', 'a_second'] {
		dir := os.join_path(module_root, module_name)
		include := os.join_path(dir, 'include')
		os.mkdir_all(include)!
		header := os.join_path(include, 'vup_order_value.h')
		headers << header
		os.write_file(header, 'static inline int vup_order_value(void) { return ${if i == 0 {
			21
		} else {
			42
		}}; }\n')!
		os.write_file(os.join_path(dir, 'value.c.v'), 'module ${module_name}\n' +
			'#flag -I"${include}"\n#include "vup_order_value.h"\n' +
			'fn C.vup_order_value() int\npub fn value() int { return C.vup_order_value() }\n')!
	}
	mut entries := []string{}
	for i, modules in [['z_first', 'a_second'], ['a_second', 'z_first']] {
		entry := os.join_path(root, 'main_${i}.v')
		entries << entry
		imports := modules.map('import ${name}.${it}\n').join('')
		os.write_file(entry, imports + 'fn main() { println(z_first.value() + a_second.value()) }\n')!
		build := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-o', os.join_path(root, 'program'),
			entry])
		assert build.exit_code == 0, build.output
		run := os.exec([os.join_path(root, 'program')])
		assert run.exit_code == 0 && run.output.trim_space() == if i == 0 { '42' } else { '84' }, run.output
	}
	requests := collect(vroot, @VEXE)!.filter(it.module.starts_with(name + '.'))
	// Header ownership is opaque, so these builds publish no refresh inventory.
	assert requests.len == 0, requests.str()
	os.write_file(headers[0], 'static inline int vup_order_value(void) { return 31; }\n')!
	for i, entry in entries {
		warm := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-show-timings', '-o',
			os.join_path(root, 'program'), entry])
		assert warm.exit_code == 0, warm.output
		assert !warm.output.contains('Caching module '), warm.output
		run := os.exec([os.join_path(root, 'program')])
		assert run.exit_code == 0 && run.output.trim_space() == if i == 0 { '62' } else { '84' }, run.output
	}
}

fn test_repeated_refresh_keeps_the_inventory_and_module_objects_warm() ! {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'vup_repeated_refresh_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := os.join_path(root, 'main.v')
	os.write_file(entry, 'import crypto.sha3\nfn main() { println(sha3.sum256("cache".bytes())) }\n')!
	mut environment := os.environ()
	for name in ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH', 'V3_CACHE_FORCE_SOURCE',
		'VTEST_ONLY_FN'] {
		environment.delete(name)
	}
	environment['V3CACHE'] = os.join_path(root, 'cache')
	environment['VEXE'] = @VEXE
	environment['V_SKIP_VVMRC'] = '1'
	mut compiler := os.new_process(@VEXE)
	compiler.set_environment(environment)
	compiler.set_args(['-cc', 'clang', '-gc', 'none', '-o', os.join_path(root, 'program'), entry])
	compiler.set_redirect_stdio()
	compiler.wait()
	assert compiler.code == 0, compiler.stdout_slurp() + compiler.stderr_slurp()
	compiler.close()
	helper_source := os.join_path(root, 'refresh_helper.v')
	os.write_file(helper_source, 'import os\nimport v.util.cache_refresh\nfn main() {\n' +
		'for _ in 0 .. 3 {\n' +
		'requests := cache_refresh.collect(os.dir(@VEXE), @VEXE)!\n' +
		'assert requests.len > 0\nprintln(requests.len)\n' +
		'cache_refresh.refresh(@VEXE, requests, verbose: true)!\n}\n}\n')!
	helper := os.join_path(root, 'refresh_helper')
	build := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', helper, helper_source])
	assert build.exit_code == 0, build.output
	mut process := os.new_process(helper)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code == 0, output
	process.close()
	counts := output.trim_space().split_into_lines().filter(it.len > 0 && it.bytes().all(it.is_digit()))
	assert counts.len == 3, output
	assert counts[0] == counts[1] && counts[1] == counts[2], output
	assert output.count('> Module cache refresh complete: 1 build configurations checked.') == 3, output
}

fn test_refresh_configuration_identifies_compiler_and_modes() {
	assert refresh_configuration(Request{ args: ['-cc', 'tcc'] }) == 'normal program; -cc tcc'
	assert refresh_configuration(Request{ args: ['-cc', 'clang', '-prod'], is_test: true }) == 'production tests; -cc clang -prod'
	assert refresh_configuration(Request{ args: ['-gc', 'none', '-g', '-cc', '/path with spaces/clang'] }) == 'normal program; -gc none -g -cc ${os.quoted_path('/path with spaces/clang')}'
	assert refresh_configuration(Request{}) == 'normal program; default compiler flags'
	flags := refresh_configuration(Request{ args: ['-cflags', '-DFIRST\n-DSECOND\r\t'] })
	assert !flags.contains('\n') && !flags.contains('\r') && !flags.contains('\t'), flags
	assert flags.contains('\\n') && flags.contains('\\r') && flags.contains('\\t'), flags
}

fn test_refresh_reports_only_children_producing_output_unless_verbose() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_quiet_refresh_${os.getpid()}')
	for name in ['first', 'second'] { os.mkdir_all(os.join_path(root, 'vlib', name))! }
	defer { os.rmdir_all(root) or {} }
	log := os.join_path(root, 'calls')
	vexe := os.join_path(root, 'v')
	os.write_file(vexe, '#!/bin/sh\n' +
		'echo called >> ' + os.quoted_path(log) + '\n' +
		'case "\$*" in\n' +
		'  *"-d noisy"*) printf "child stdout\\n"; printf "child stderr\\n" >&2;;\n' +
		'  *"-d stderr"*) printf "child stderr\\n" >&2;;\n' +
		'  *"-d fail"*) exit 7;;\n' +
		'esac\nexit 0\n')!
	os.chmod(vexe, 0o755)!
	source := os.join_path(root, 'helper.v')
	os.write_file(source, 'import os\nimport v.util.cache_refresh\nfn main() {\n' +
		'mode := os.args[1]\n' +
		'kind := if mode == "mixed" { "noisy" } else if mode in ["stderr", "fail"] { mode } else { "quiet" }\n' +
		'first := cache_refresh.Request{module: "first", args: ["-d", "quiet"]}\n' +
		'second := cache_refresh.Request{module: "second", args: ["-d", kind]}\n' +
		'cache_refresh.refresh("${vexe}", [first, first, second], verbose: mode == "verbose")!\n}\n')!
	helper := os.join_path(root, 'helper')
	build := os.exec([@VEXE, '-nocache', '-gc', 'none', '-o', helper, source])
	assert build.exit_code == 0, build.output
	for mode in ['quiet', 'mixed', 'stderr', 'verbose', 'fail'] {
		os.write_file(log, '')!
		result := os.exec([helper, mode])
		output := result.output
		// Quiet checks and duplicate elimination must still validate both configurations.
		assert os.read_file(log)!.split_into_lines().len == 2, output
		match mode {
			'quiet' {
				assert result.exit_code == 0 && output == '', output
			}
			'mixed', 'stderr' {
				assert result.exit_code == 0, output
				assert output.count('> Refreshing module cache (') == 1, output
				assert !output.contains('> Refreshing module cache ['), output
				assert !output.contains('-d quiet'), output
				flag := if mode == 'mixed' { 'noisy' } else { 'stderr' }
				assert output.starts_with('> Refreshing module cache (normal program; -d ${flag}).\n'), output
				if mode == 'mixed' {
					assert output.contains('child stdout\n'), output
				}
				assert output.contains('child stderr\n'), output
				assert output.contains('> Module cache refresh complete: 2 build configurations checked.'), output
			}
			'verbose' {
				assert result.exit_code == 0, output
				assert output.count('> Refreshing module cache [') == 2, output
				assert output.contains('[1/3]') && output.contains('[3/3]'), output
				assert output.contains('> Module cache refresh complete: 2 build configurations checked.'), output
			}
			'fail' {
				assert result.exit_code != 0, output
				assert output.contains('> Module cache refresh failed [3/3] (normal program; -d fail; exit 7).'), output
				assert !output.contains('> Module cache refresh complete:'), output
			}
			else {}
		}
	}
}
