module time

import os

fn test_darwin_clocks_keep_native_declarations_without_sdk_headers() ! {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'headerless_clock_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import time
fn main() {
	assert time.now().year > 1970
	assert time.utc().year > 1970
	first := time.sys_mono_now()
	assert time.sys_mono_now() >= first
	println("clock OK")
}
')!
	output := os.join_path(root, 'clocks')
	for nocache in [true, false, false] {
		mut args := [@VEXE, '-b', 'c', '-cc', 'clang', '-gc', 'none', '-cstrict',
			'-no-retry-compilation', '-o', output]
		if nocache {
			args << '-nocache'
		}
		args << ['run', source]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output == 'clock OK\n', result.output
		code := os.read_file(output + '.c')!
		lines := code.split_into_lines()
		// The heap-mode guard contains only preprocessor checks, not SDK declarations.
		heap_guard := '#include "${os.join_path(@VEXEROOT, 'vlib', 'builtin', 'track_heap_checks.h').replace('\\', '/')}"'
		assert !lines.any(it.trim_space().starts_with('#include') && it.trim_space() != heap_guard), 'clock bindings must retain standalone native declarations'
		assert lines.any(it.starts_with('u64 mach_absolute_time(') && it.ends_with(';'))
		assert lines.any(it.starts_with('i32 mach_timebase_info(') && it.ends_with(';'))
	}
}
