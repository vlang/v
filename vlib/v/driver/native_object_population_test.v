module driver

import os
import time
import v.cmdexec
import v.pref

fn testsuite_begin() {
	if root := os.getenv_opt('V_NATIVE_OBJECT_POPULATION_TEST') {
		run_native_object_population_helper(root)
	}
}

fn run_native_object_population_helper(root string) {
	index := os.getenv('V_NATIVE_OBJECT_POPULATION_INDEX')
	os.write_file(os.join_path(root, 'ready_${index}'), '') or { panic(err) }
	mut ready := false
	for _ in 0 .. 1000 {
		if os.is_file(os.join_path(root, 'start')) {
			ready = true
			break
		}
		time.sleep(10 * time.millisecond)
	}
	if !ready {
		exit(2)
	}
	mut stats := CObjectCacheStats{}
	// Exercise the cache's lock and publication protocol with an instrumented compiler.
	object := compile_c_object_with_manifest(os.join_path(root, 'probe.o'), os.join_path(root,
		'probe.c'), os.join_path(root, 'cc'), ['-std=c11', '-w', '-x', 'c'], pref.host_target(),
		root, '', mut stats) or {
		eprintln(err)
		exit(1)
	}
	os.write_file(os.join_path(root, 'object_${index}'), object) or { panic(err) }
	exit(0)
}

fn run_native_object_population_wave(root string) ![]int {
	os.rm(os.join_path(root, 'start')) or {}
	mut processes := []&os.Process{}
	defer {
		for mut process in processes {
			if process.is_alive() {
				process.signal_kill()
			}
			process.wait()
			process.close()
		}
	}
	for index in 0 .. 3 {
		os.rm(os.join_path(root, 'ready_${index}')) or {}
		os.rm(os.join_path(root, 'object_${index}')) or {}
		mut environment := os.environ()
		environment['V_NATIVE_OBJECT_POPULATION_TEST'] = root
		environment['V_NATIVE_OBJECT_POPULATION_INDEX'] = index.str()
		environment['VTMP'] = os.join_path(root, 'tmp')
		mut process := os.new_process(os.executable())
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		processes << process
	}
	mut ready := false
	for _ in 0 .. 1000 {
		if [0, 1, 2].all(os.is_file(os.join_path(root, 'ready_${it}'))) {
			ready = true
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert ready, 'native object helpers did not reach the start gate'
	os.write_file(os.join_path(root, 'start'), '')!
	mut codes := []int{}
	for mut process in processes {
		process.wait()
		codes << process.code
		if process.code != 0 {
			eprintln(process.stdout_slurp() + process.stderr_slurp())
		}
	}
	return codes
}

fn test_concurrent_native_objects_reuse_builds_and_recover_after_changes_and_failure() ! {
	$if windows {
		return // The counting compiler wrapper below uses a POSIX shell.
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'native_object_population_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	log := os.join_path(root, 'compiles')
	fail_next := os.join_path(root, 'fail-next')
	os.write_file(os.join_path(root, 'cc'), '#!/bin/sh
for arg do
    if [ "$arg" = -c ]; then
        printf "compile\\n" >> ${os.quoted_path(log)}
        sleep 0.2
        if [ -f ${os.quoted_path(fail_next)} ]; then
            rm ${os.quoted_path(fail_next)}
            exit 42
        fi
    fi
done
exec ${os.quoted_path(clang)} "$@"
')!
	os.chmod(os.join_path(root, 'cc'), 0o755)!
	os.write_file(os.join_path(root, 'probe.c'), '#include "value.h"\nint probe(void) { return VALUE; }\n')!
	os.write_file(os.join_path(root, 'main.c'), 'int probe(void); int main(void) { return probe() != EXPECTED; }\n')!
	for wave, value in [77, 88, 99] {
		os.write_file(os.join_path(root, 'value.h'), '#define VALUE ${value}\n')!
		if wave == 2 {
			os.write_file(fail_next, '')!
		}
		codes := run_native_object_population_wave(root)!
		assert codes.filter(it == 0).len == if wave == 2 { 2 } else { 3 }, codes.str()
		assert codes.filter(it == 1).len == if wave == 2 { 1 } else { 0 }, codes.str()
		expected_compiles := if wave == 2 { 4 } else { wave + 1 }
		compiles := os.read_lines(log)!.len
		assert compiles == expected_compiles, 'expected ${expected_compiles} native compiles, got ${compiles}'
		object := os.read_file(os.join_path(root, 'object_${codes.index(0)}'))!
		for index, code in codes {
			if code == 0 {
				assert os.read_file(os.join_path(root, 'object_${index}'))! == object
			}
		}
		program := os.join_path(root, 'program')
		linked := cmdexec.run(clang, ['-DEXPECTED=${value}', os.join_path(root, 'main.c'), object,
			'-o', program])
		assert linked.exit_code == 0, linked.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
	assert run_native_object_population_wave(root)! == [0, 0, 0]
	assert os.read_lines(log)!.len == 4, 'warm reuse must not compile a native object'
}

fn test_native_manifest_waiter_refreshes_dependency_digests() ! {
	root := os.join_path(os.vtmp_dir(), 'native_object_waiter_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	object := os.join_path(root, 'probe.o')
	header := os.join_path(root, 'value.h')
	manifest := os.join_path(root, 'probe.manifest')
	// Object contents are checked by the compile/link regression above.
	os.write_file(object, 'object')!
	os.write_file(header, '#define VALUE 1\n')!
	mut writer := CObjectCacheStats{}
	write_c_object_manifest(manifest, object, &CObjectDependencies{ files: [header] }, mut writer)!
	mut waiter := CObjectCacheStats{}
	assert valid_c_object_manifest(manifest, mut waiter) != none
	os.write_file(header, '#define VALUE 2\n')!
	assert valid_c_object_manifest_refreshed(manifest, true, mut waiter) == none
}

fn test_c_object_cache_detects_input_change_during_compile() ! {
	$if windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'native_object_input_race_${os.getpid()}')
	cache := os.join_path(root, 'cache')
	os.mkdir_all(cache)!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'value.h')
	source := os.join_path(root, 'helper.c')
	compiler := os.join_path(root, 'cc')
	os.write_file(header, '#define RACE_VALUE 1\n')!
	os.write_file(source, '#include "value.h"\nint race_value(void) { return RACE_VALUE; }\n')!
	// The wrapper changes the dependency only after the object has been built.
	os.write_file(compiler, '#!/bin/sh
${os.quoted_path(clang)} "$@" || exit $?
for arg do
	if [ "$arg" = "-c" ]; then
		printf "#define RACE_VALUE 7\\n" > ${os.quoted_path(header)}
	fi
done
')!
	os.chmod(compiler, 0o755)!
	mut stats := CObjectCacheStats{}
	object := compile_c_object_with_manifest(os.join_path(root, 'helper.o'), source, compiler,
		['-x', 'c'], pref.host_target(), root, cache, mut stats)!
	assert stats.input_snapshot_races == 1
	assert os.dir(object) == root
	assert os.walk_ext(cache, '.o').len == 0
	main := os.join_path(root, 'main.c')
	out := os.join_path(root, 'program')
	os.write_file(main, 'int race_value(void); int main(void) { return race_value() != 1; }\n')!
	linked := cmdexec.run(clang, [main, object, '-o', out])
	assert linked.exit_code == 0, linked.output
	ran := cmdexec.run(out, [])
	assert ran.exit_code == 0, ran.output
}
