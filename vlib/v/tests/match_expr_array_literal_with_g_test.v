import os

const vexe = @VEXE

fn test_match_expr_array_literal_compiles_with_g() {
	test_dir := os.join_path(os.vtmp_dir(), 'match_expr_array_g_test_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source := os.join_path(test_dir, 'test.v')
	out_c := os.join_path(test_dir, 'test.c')
	os.write_file(source, "import os

fn pick() []string {
	home := os.home_dir()
	return match os.user_os() {
		'windows' {
			[os.join_path(os.getenv('LOCALAPPDATA'), 'a'), os.join_path(home, 'b')]
		}
		else {
			[]string{}
		}
	}
}

fn main() {
	println(pick())
}
")!
	res :=
		os.execute('${os.quoted_path(vexe)} -g -o ${os.quoted_path(out_c)} -b c ${os.quoted_path(source)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(out_c)!
	// Ensure no #line directive is glued to the following expression (missing newline).
	assert !generated.contains('"builtin__new_array'), 'found #line directive glued to expression'
}
