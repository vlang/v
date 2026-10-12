module cache_refresh

import os

fn test_inventory_deduplicates_import_order() ! {
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
	record(dir, root, ['-cc', 'cc'], false, {
		'other':  [other]
		'sample': [source]
	}, [], '', false)!
	assert os.ls(dir)! == before
	// Old records with another import ordering have the same inventory identity.
	a := Request{ module: 'sample', imports: ['sample', 'other'] }
	b := Request{ ...a, imports: ['other', 'sample'] }
	assert encode_request(a) == encode_request(b)
}

fn test_synthetic_refresh_replays_each_import_set_once() ! {
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
	// Reordering the same inventory imports must not start another child.
	refresh(vexe, [requests[1], Request{ ...requests[2], imports: ['other', 'sample'] }])!
	assert os.read_file(log)!.count('fn main() {}') == 2
	// Compiler modes, test mode, source flags and import sets remain separate.
	refresh(vexe, [
		Request{ ...requests[1], args: ['-prod'] },
		Request{ ...requests[1], is_test: true },
		Request{ ...requests[1], show_test_stats: true },
		Request{ ...requests[1], project_flags: [FlagGroup{ flags: ['-DOTHER'] }] },
		Request{ module: 'sample', imports: ['sample'] },
	])!
	after := os.read_file(log)!
	assert after.count('fn main() {}') == 6, after
	assert after.count('fn test_cache_refresh() {}') == 1, after
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
		'cache_refresh.refresh(@VEXE, requests)!\n}\n}\n')!
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
	counts := output.trim_space().split_into_lines()
	assert counts.len == 3, output
	assert counts[0] == counts[1] && counts[1] == counts[2], output
}
