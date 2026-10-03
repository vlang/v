import os

fn test_borrowed_array_parameter_indexing_survives_generic_annotation() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_container_annotation_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn identity[T](value T) T { return value }
fn skip_table(literal &[]u8) []usize {
	m := usize(literal.len)
	mut skip := []usize{len: 256, init: m}
	for i := 0; i + 1 < literal.len; i++ {
		skip[int(literal[i])] = m - 1 - usize(i)
	}
	return skip
}
fn main() {
	assert identity[int](7) == 7
	literal := "abc".bytes()
	skip := skip_table(&literal)
	assert skip[int(`a`)] == 2
	assert skip[int(`b`)] == 1
	assert skip[int(`c`)] == 3
	assert literal.bytestr() == "abc"
}
')!
	for ownership in ['', '-ownership -d ownership'] {
		for parallel in ['', '-no-parallel'] {
			binary := os.join_path(root, 'case')
			result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache ${ownership} ${parallel} -o ${os.quoted_path(binary)} run ${os.quoted_path(source)}')
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_raw_pointer_indexing_still_requires_unsafe() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_container_annotation_raw_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn identity[T](value T) T { return value }
fn value(pointer &u8) u8 {
	return pointer[0]
}
fn main() {
	assert identity[int](7) == 7
	byte := u8(1)
	assert value(&byte) == 1
}
')!
	for ownership in ['', '-ownership -d ownership'] {
		for parallel in ['', '-no-parallel'] {
			binary := os.join_path(root, 'case')
			result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache ${ownership} ${parallel} -o ${os.quoted_path(binary)} ${os.quoted_path(source)}')
			assert result.exit_code != 0, result.output
			assert result.output.contains('pointer indexing is only allowed in `unsafe` blocks'), result.output
		}
	}
}
