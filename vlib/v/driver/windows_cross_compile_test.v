module driver

import os
import v.cmdexec

const windows_cross_source = 'import os\n\nfn main() {\n\tprintln(os.args.len)\n}\n'

fn windows_cross_test_dir(name string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_windows_cross_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	return dir
}

// run_windows_cross_build builds `source` like a user without VFLAGS would: a
// `-cc` from VFLAGS selects another compiler and target architecture.
fn run_windows_cross_build(args []string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	mut vexe := @VEXE
	if os.base(vexe) in ['v1_fallback', 'v1_fallback.exe'] {
		vexe = os.join_path(os.dir(vexe), 'v' + $if windows { '.exe' } $else { '' })
	}
	return cmdexec.run(vexe, args)
}

fn test_unsupported_windows_target_exits_with_its_error() {
	$if windows {
		return
	}
	dir := windows_cross_test_dir('unsupported')
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, windows_cross_source)!
	// The link step rejects the target after C generation, while helper threads
	// still release its arenas. The process used to crash at exit with a
	// segmentation fault in most runs, so try a few times.
	for _ in 0 .. 3 {
		res := run_windows_cross_build(['-nocache', '-os', 'windows', '-arch', 'arm64', '-o',
			os.join_path(dir, 'main.exe'), source])
		assert res.exit_code == 1, res.output
		assert res.output.contains('linking target windows/arm64 from host'), res.output
		assert !res.output.contains('signal'), res.output
	}
}

fn test_windows_target_builds_an_amd64_executable_with_mingw() {
	$if windows {
		return
	}
	os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') or {
		eprintln('skipping: x86_64-w64-mingw32-gcc is not installed')
		return
	}
	dir := windows_cross_test_dir('mingw')
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	output := os.join_path(dir, 'main.exe')
	os.write_file(source, windows_cross_source)!
	res := run_windows_cross_build(['-nocache', '-os', 'windows', '-o', output, source])
	assert res.exit_code == 0, res.output
	exe := os.read_bytes(output)!
	assert exe.len > 0x40 && exe[0] == `M` && exe[1] == `Z`
	pe := int(u32(exe[0x3c]) | (u32(exe[0x3d]) << 8) | (u32(exe[0x3e]) << 16) | (u32(exe[0x3f]) << 24))
	assert pe + 6 <= exe.len
	assert exe[pe..pe + 4] == [u8(`P`), `E`, 0, 0]
	// IMAGE_FILE_MACHINE_AMD64, also on an arm64 host.
	assert u16(exe[pe + 4]) | (u16(exe[pe + 5]) << 8) == 0x8664
}
