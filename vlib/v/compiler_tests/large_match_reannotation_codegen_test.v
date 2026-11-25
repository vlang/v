// vtest ownership
import os
import v.cmdexec
import v.tempname

// Match lowering produces an else-if chain. Post-transform type reannotation
// must walk very large generated tables without overflowing the native stack.
fn test_large_match_reannotation_does_not_overflow() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_large_match_reannotation_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_large_match_reannotation.v')
	out := os.join_path(root, 'v3_large_match_reannotation_program')
	mut branches := []string{cap: 1800}
	for i in 0 .. 1800 {
		branches << "\t\t'key_${i}' { return ${i} }"
	}
	source := 'module main

fn identity[T](value T) T {
\treturn value
}

fn classify(value string) int {
\tmatch value {
${branches.join('\n')}
\t\telse { return -1 }
\t}
}

fn main() {
\tassert classify(identity[string]("key_1799")) == 1799
}
'
	os.write_file(src, source) or { panic(err) }
	compile := cmdexec.run_in_merged_with_env(compiler, [src, '-d', 'ownership', '-b', 'c', '-o',
		out], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
