import os

const vexe = @VEXE
const vfmt_test_tdir = os.join_path(os.vtmp_dir(), 'vfmt_test_25005')

fn testsuite_begin() {
	os.rmdir_all(vfmt_test_tdir) or {}
	os.mkdir_all(vfmt_test_tdir)!
}

fn testsuite_end() {
	os.rmdir_all(vfmt_test_tdir) or {}
}

fn test_fmt_keeps_invalid_assert_source_unchanged() {
	source_path := os.join_path(vfmt_test_tdir, 'invalid_assert_message.v')
	original := "fn main() {\n\tassert false 'bye'\n}\n"
	os.write_file(source_path, original)!

	res := os.execute('${os.quoted_path(vexe)} fmt -w ${os.quoted_path(source_path)}')

	assert res.exit_code != 0, res.output
	assert res.output.contains('unexpected string `bye`, expecting `,`'), res.output
	assert os.read_file(source_path)! == original
}
