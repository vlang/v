import os

const vlibc_os_program = "import os

fn main() {
	path := os.args[1]
	missing := os.args[2]
	link := os.args[3]
	mut bytes := []u8{len: 65537, init: u8(index % 251)}
	assert !os.exists(link)
	os.write_bytes(path, bytes)!
	assert os.exists(path) && os.exists(link)
	assert !os.exists(missing)
	assert os.read_bytes(path)! == bytes
	assert os.read_file(path)!.bytes() == bytes
	binary := [u8(97), 0, 98].bytestr()
	os.write_file(path, binary)!
	assert os.read_file(path)! == binary
	os.write_file(path, '')!
	assert os.read_file(path)! == ''
	assert os.read_bytes(path)!.len == 0
	mut write_failed := false
	os.write_file(missing + '/child', 'x') or {
		assert err.code() == 2
		write_failed = true
	}
	assert write_failed
	os.read_file(missing) or {
		assert err.code() == 2
		println('file operations ok')
		return
	}
	assert false, 'missing file read unexpectedly succeeded'
}
"

fn test_vlibc_os_file_operations() {
	$if !(linux || macos || freebsd || openbsd || netbsd || dragonfly || solaris) {
		return
	}
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_os_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, vlibc_os_program)!
	path := os.join_path(root, 'data.bin')
	missing := os.join_path(root, 'missing')
	link := os.join_path(root, 'link')
	os.symlink(path, link)!
	for freestanding in [false, true] {
		for optimized in [false, true] {
			exe := os.join_path(root, 'main_${freestanding}_${optimized}')
			mut args := [@VEXE, '-nocache', '-d', 'vlibc']
			if freestanding { args << '-freestanding' }
			if optimized { args << '-prod' }
			args << ['-o', exe, source]
			build := os.exec(args)
			assert build.exit_code == 0, '${freestanding}/${optimized}: ${build.output}'
			run := os.exec(['/bin/sh', '-c', 'umask 0; exec "$@"', 'sh', exe, path, missing, link])
			assert run.exit_code == 0, '${freestanding}/${optimized}: ${run.output}'
			assert run.output == 'file operations ok\n', run.output
			assert os.stat(path)!.mode & 0o777 == 0o666
			os.rm(path)!
		}
	}
}

fn test_vlibc_rejects_unsupported_backend_and_target() {
	root := os.join_path(os.vtmp_dir(), 'vlibc_target_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { println(os.args.len) }')!
	for target_flags in [['-os', 'windows'], ['-b', 'js'], ['-b', 'fastc'],
		['-os', 'solaris', '-arch', 'arm64']] {
		for define in ['vlibc', 'vlibc=1'] {
			mut args := [@VEXE, '-nocache', '-d', define]
			args << target_flags
			args << ['-o', os.join_path(root, 'out.c'), source]
			result := os.exec(args)
			assert result.exit_code != 0
			assert result.output.contains('`-d vlibc` requires the C backend and a supported Unix amd64/arm64 target'), result.output
		}
	}
}
