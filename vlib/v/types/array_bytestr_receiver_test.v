module main

import os
import v.cmdexec

// `bytestr` exists only for arrays of bytes. Accepted for other element types,
// it reached the backends with the wrong layout: the C backend could not compile
// the result and the arm64 backend read an array of runes as bytes.
fn test_bytestr_is_rejected_for_non_byte_arrays() {
	dir := os.join_path(os.vtmp_dir(), 'array_bytestr_receiver_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'fn main() {
	println([`a`, `b`].bytestr())
}
')!
	result := cmdexec.run(@VEXE, ['-o', os.join_path(dir, 'main'), source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('bytestr'), result.output
}

fn test_bytestr_is_accepted_for_bytes_and_their_aliases() {
	dir := os.join_path(os.vtmp_dir(), 'array_bytestr_alias_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'type Octet = u8

fn main() {
	println([u8(65), 66].bytestr())
	println([Octet(67)].bytestr())
}
')!
	result := cmdexec.run(@VEXE, ['-o', os.join_path(dir, 'main'), source])
	assert result.exit_code == 0, result.output
	run := cmdexec.run(os.join_path(dir, 'main'), []string{})
	assert run.output.trim_space() == 'AB\nC', run.output
}
