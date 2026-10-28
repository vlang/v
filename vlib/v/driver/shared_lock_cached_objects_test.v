module driver

import os
import v.cmdexec

// `lock` on a `shared` parameter inside a spawned function.
const shared_lock_program = 'struct Acc {
mut:
	n int
}

fn add(shared a Acc) {
	lock a {
		a.n++
	}
}

fn main() {
	shared a := Acc{}
	t := spawn add(shared a)
	t.wait()
	rlock a {
		println(a.n)
	}
}
'

// This program hung forever when its module objects came from the object cache.
// The cached `sync` object had been built with V's fallback declaration of
// `pthread_rwlock_t` (256 bytes), while the program itself used the system header
// (56 bytes). The two disagreed about where `sync.RwMutex.inited` is, so
// `RwMutex.lazy_init` read past the end of the shared struct and spun on garbage.
// Building the same source twice, with the cache of the first build in place, is
// what selects the cached objects; the second program must still print `1`.
fn test_lock_on_a_shared_parameter_in_a_spawned_fn_with_cached_module_objects() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'shared_lock_cached_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	cache := os.join_path(root, 'cache')
	env := {
		'VTMP':                          cache
		'VCACHE':                        cache
		'V_C_ERROR_BUG_REPORT_DISABLED': '1'
	}
	for name in ['first', 'second'] {
		source := os.join_path(root, '${name}.v')
		program := os.join_path(root, name)
		os.write_file(source, shared_lock_program)!
		build := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-o', program, source],
			root, env)
		assert build.exit_code == 0, build.output
		outcome := cmdexec.run_with_timeout(program, [], 10_000)
		assert outcome.exit_code == 0, '${name}: the program did not finish or failed (exit code ${outcome.exit_code}): ${outcome.output}'
		assert outcome.output.trim_space() == '1', '${name}: expected 1, got: ${outcome.output}'
	}
}
