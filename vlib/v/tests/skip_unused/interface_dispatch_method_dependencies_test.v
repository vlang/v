import os

const vexe = @VEXE

fn test_skip_unused_walks_mut_interface_dispatch_method_dependencies() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27404')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_27404.v')
	binary_path := os.join_path(tmp_dir, 'issue_27404')
	source := [
		'module main',
		'',
		'interface Visitor {',
		'\tvisit()',
		'}',
		'',
		'struct Han {',
		'mut:',
		'\tn int',
		'}',
		'',
		'fn helper(s string) {',
		'\tprintln(s)',
		'}',
		'',
		'fn (mut h Han) visit() {',
		"\thelper('t')",
		'}',
		'',
		'fn run(mut v Visitor) {',
		'\tv.visit()',
		'}',
		'',
		'fn main() {',
		'\tmut h := Han{}',
		'\trun(mut h)',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(binary_path)} ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
}
