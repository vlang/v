import os

const target_libc_errno_vexe = @VEXE

// With -target-libc-headers the target's <errno.h> owns bodyless errno bindings.
// Restating them as `&i32` conflicts where int32_t is `long`, as with newlib's
// `int *__errno(void)` on arm-none-eabi. The bare runtime's own export remains.
fn test_target_libc_headers_leave_the_errno_accessor_to_the_target() {
	root := os.join_path(os.temp_dir(), 'v3_target_libc_errno_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	input := os.join_path(root, 'main.c.v')
	os.write_file(input, 'module main

fn C.puts(s &char) int

@[export: "blob_entry"]
fn blob_entry() {
	C.puts(c"hi")
}

fn main() {}
') or {
		panic(err)
	}
	out := os.join_path(root, 'out.c')
	// -target-libc-headers refuses Windows targets, and the target OS defaults to the host's
	result := os.exec([target_libc_errno_vexe, '-new-compiler', '-no-retry-compilation', '-nocache',
		'-os', 'linux', '-freestanding', '-arch', 'arm32', '-target-libc-headers', '-gc', 'none',
		'-no-bounds-checking', '-enable-globals', '-o', out, input])
	assert result.exit_code == 0, result.output
	code := os.read_file(out) or { panic(err) }
	assert code.contains('#include <errno.h>'), code
	for name in ['__errno', '___errno', '__errno_location', '__error', '__get_errno_ptr', '_errno',
		'_errnop'] {
		// Linux's bare runtime supplies one accessor. Its declaration and body
		// must remain without a second declaration from the bodyless C binding.
		expected := if name == '__errno_location' { 1 } else { 0 }
		assert code.count(' ${name}(void);') == expected, name
		assert code.count(' ${name}(void) {') == expected, name
	}
	gcc := os.find_abs_path_of_executable('arm-none-eabi-gcc') or { return }
	object := os.join_path(root, 'out.o')
	compiled := os.exec([gcc, '-c', '-mcpu=cortex-m7', '-mthumb', '-ffreestanding', '-o', object,
		out])
	assert compiled.exit_code == 0, compiled.output
}
