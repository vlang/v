module types

import v.pref

// The checker must agree with the driver (pref) on which files are C-backend tests.
// When it does not, a `test_` fn in an architecture-qualified test file such as
// `foo_test.arm64.v` cannot use `!`, even though the driver compiles it as a test.
fn test_is_c_backend_test_file_matches_pref() {
	names := ['foo_test.v', 'foo_test.c.v', 'foo_test.amd64.v', 'foo_test.arm64.v', 'foo_test.x64.v',
		'foo_test.riscv64.v', 'foo_test.js.v', 'foo_test.wasm.v', 'foo_test.native.v',
		'foo_windows_test.v', 'foo_notd_x_test.v', 'foo.v', 'foo_test.txt', 'foo_test', 'foo.arm64.v',
		'test.arm64.v']
	for name in names {
		assert is_c_backend_test_file(name) == pref.is_test_file_for_backend(name, 'c'), name
		assert is_c_backend_test_file('dir/sub/' + name) == pref.is_test_file_for_backend(name,
			'c'), name
	}
	// `_test.vv` is a checker-only spelling that pref does not know about.
	assert is_c_backend_test_file('foo_test.vv')
}
