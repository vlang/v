import os
import rand

const vexe = @VEXE

// Regression test for vlang/v#27178: when a V `-shared` library is built
// with `-gc boehm`, the generated C must call `GC_allow_register_threads()`
// during library init. Without it, host-spawned threads (Rust workers,
// JNI threads, C# tasks, ...) that later enter V code cause libgc to abort
// with `"Collecting from unknown thread"` on the first collection.
fn test_shared_library_boehm_emits_allow_register_threads() {
	workdir := os.join_path(os.vtmp_dir(), 'v_shared_gc_threads_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	lib_src := os.join_path(workdir, 'lib.v')
	lib_c := os.join_path(workdir, 'lib.c')
	os.write_file(lib_src, [
		'module main',
		'',
		"@[export: 'libfoo_ping']",
		'fn ping() int {',
		'\treturn 1',
		'}',
	].join('\n')) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} -gc boehm -shared -o ${os.quoted_path(lib_c)} ${os.quoted_path(lib_src)}')
	assert res.exit_code == 0, 'shared-lib codegen failed:\n${res.output}'
	c_src := os.read_file(lib_c) or { panic(err) }
	// The call must appear inside `_vinit`, not in a separate constructor:
	// `_vinit_caller` already runs `GC_INIT()` via gen_shared_library_boehm_init,
	// and calls `_vinit(0,0)` right after, so `GC_allow_register_threads()`
	// inside `_vinit` is sequenced correctly.
	vinit_start := c_src.index('void _vinit(int ___argc, voidptr ___argv) {') or {
		assert false, '`_vinit` definition not found in generated C'
		return
	}
	// `_vinit`'s body has no nested top-level `\n}\n`; the next one closes it.
	vinit_end := c_src.index_after('\n}\n', vinit_start) or {
		assert false, '`_vinit` body terminator not found in generated C'
		return
	}
	vinit_body := c_src[vinit_start..vinit_end]
	assert vinit_body.contains('GC_allow_register_threads();'), 'expected `GC_allow_register_threads()` inside `_vinit`, got:\n${vinit_body}'
	assert vinit_body.contains('#if defined(_VGCBOEHM) && defined(GC_THREADS)'), 'expected `GC_THREADS` guard around the call, got:\n${vinit_body}'
}
