// vtest ownership
import os

fn test_borrowed_array_indexing_and_match_postfixes() {
	root := os.join_path(os.vtmp_dir(), 'borrowed_container_annotation_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn identity[T](value T) T { return value }
fn skip_table(literal &[]u8) []uint {
	m := uint(literal.len)
	mut skip := []uint{len: 256, init: m}
	for i := 0; i + 1 < literal.len; i++ {
		skip[int(literal[i])] = m - 1 - uint(i)
	}
	return skip
}
fn main() {
	check_match_postfixes()
	assert identity[int](7) == 7
	literal := "abc".bytes()
	skip := skip_table(&literal)
	assert skip[int(`a`)] == 2
	assert skip[int(`b`)] == 1
	assert skip[int(`c`)] == 3
	assert literal.bytestr() == "abc"
}
')!
	os.write_file(os.join_path(root, 'match_postfixes.v'), 'struct Holder { value u8 }
fn (holder &Holder) byte() u8 { return holder.value }
fn index() int { return 1 }
fn match() !string { return "abc" }
fn check_match_postfixes() {
	text := "abc"
	pointer := &text
	mut hits := 0
	match (*pointer)[0] { `a` { hits++ } else { assert false } }
	result := match (((*pointer)))[index()] { `b` { 42 } else { 0 } }
	assert result == 42
	match (*pointer)[0..2][1] { `b` { hits++ } else { assert false } }
	holder := Holder{value: `a`}
	holder_pointer := &holder
	match (*holder_pointer).value { `a` { hits++ } else { assert false } }
	match (*holder_pointer).byte() { `a` { hits++ } else { assert false } }
	assert hits == 4
	called := match() or { panic(err) }
	assert called[0] == `a`
	assert (match() or { panic(err) }).bytes()[1] == `b`
}
')!
	for ownership in [[]string{}, ['-ownership', '-d', 'ownership']] {
		for mode in [[]string{}, ['-no-parallel']] {
			result := os.exec([@VEXE, '-no-memory-limit', '-nocache', ...ownership, ...mode, '-o',
				os.join_path(root, 'case'), 'run', root])
			assert result.exit_code == 0, '${ownership} ${mode}: ${result.output}'
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
			result := os.execute('${os.quoted_path(@VEXE)} -no-memory-limit -nocache ${ownership} ${parallel} -o ${os.quoted_path(binary)} ${os.quoted_path(source)}')
			assert result.exit_code != 0, result.output
			assert result.output.contains('pointer indexing is only allowed in `unsafe` blocks'), result.output
		}
	}
}
