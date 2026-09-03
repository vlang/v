// vtest build: !windows
import os

const vexe = @VEXE
const fixture_dir = os.join_path(@VMODROOT, 'vlib', 'v', 'tests', 'testdata',
	'usecache_multi_module')
const root = os.join_path(os.vtmp_dir(), 'v_usecache_multi_module_${os.getpid()}')
const proj_dir = os.join_path(root, 'proj')
const cache_dir = os.join_path(root, 'cache')
const tmp_dir = os.join_path(root, 'vtmp')

// Regression test for https://github.com/vlang/v/issues/27592 and the wider
// family of -usecache correctness bugs it exposed.
//
// With -usecache, the program TU is compiled with skip_unused ON while each
// cached module (built via `v build-module`) is compiled with skip_unused OFF,
// so the two used to disagree about what is defined vs referenced (duplicate
// or undefined symbols at link time) and about sum type index numbering
// (`x is T` silently misdispatching across TUs). Cached modules also carried
// their consts as zeroed per-TU statics (e.g. `max_int == 0` inside builtin.o,
// making `array.push` panic on any growth), and closures created inside a
// cached module ran on an uninitialized trampoline pool.
//
// The fixture is a small multi-module project exercising, across the
// program-TU/cached-module boundary: sumtype `is`, interface dispatch,
// closures, module consts / array growth, and float formatting (strconv's
// const tables inside the cached builtin object). It is compiled here as a
// normal program and as a test build, each with a cold and a warm cache.
fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	// A regression would surface as a C error; fail fast instead of invoking
	// the bug report machinery.
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
	os.rmdir_all(root) or {}
	os.mkdir_all(cache_dir) or { panic(err) }
	os.mkdir_all(tmp_dir) or { panic(err) }
	os.cp_all(fixture_dir, proj_dir, true) or { panic(err) }
	// Isolate the module cache, so cold vs warm behavior is deterministic and
	// the user's real cache is left untouched.
	os.setenv('VCACHE', cache_dir, true)
	os.setenv('VTMP', tmp_dir, true)
}

fn testsuite_end() {
	os.rmdir_all(root) or {}
}

fn test_usecache_program_cold_and_warm() {
	for label in ['cold', 'warm'] {
		res := os.execute('${os.quoted_path(vexe)} -usecache run ${os.quoted_path(proj_dir)}')
		assert res.exit_code == 0, '${label} -usecache run failed:\n${res.output}'
		assert res.output.trim_space() == 'OK', '${label} -usecache run output:\n${res.output}'
	}
}

fn test_usecache_test_build_cold_and_warm() {
	// Start from a fresh cache, so the test-build path is also exercised cold
	// (it caches the modules of the _test.v file itself, in addition to the
	// imported ones).
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir) or { panic(err) }
	for label in ['cold', 'warm'] {
		res := os.execute('${os.quoted_path(vexe)} -usecache test ${os.quoted_path(proj_dir)}')
		assert res.exit_code == 0, '${label} -usecache test failed:\n${res.output}'
	}
}
