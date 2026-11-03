module bytes

import os

fn test_repeat_rejects_length_overflow() {
	dir := os.join_path(os.vtmp_dir(), 'bytes_overflow_test_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'helper.v')
	exe := os.join_path(dir, 'helper' + $if windows { '.exe' } $else { '' })
	os.write_file(src, 'import bytes\nfn main() {\n println(bytes.repeat([u8(1), 2], 2147483647))\n}\n') or {
		panic(err)
	}
	compiled := os.exec([@VEXE, '-o', exe, src])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([exe])
	assert result.exit_code != 0
	assert result.output.contains('bytes.repeat: length overflow'), result.output
}
