import os

fn test_parenthesized_match_scrutinee_postfixes() {
	root := os.join_path(os.vtmp_dir(), 'match_parenthesized_postfix_driver_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Holder { value u8 }
fn (holder &Holder) byte() u8 { return holder.value }
fn index() int { return 1 }
fn match() !string { return "abc" }
fn main() {
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
	for ownership in ['', '-ownership -d ownership'] {
		for parallel in ['', '-no-parallel'] {
			binary := os.join_path(root, 'case')
			result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-memory-limit -nocache ${ownership} ${parallel} -o ${os.quoted_path(binary)} run ${os.quoted_path(source)}')
			assert result.exit_code == 0, result.output
		}
	}
}
