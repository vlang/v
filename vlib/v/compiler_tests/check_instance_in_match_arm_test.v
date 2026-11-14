import os

const vexe = @VEXE

// A check does not lower `match`, so the instances that an arm asks for have to be
// found without that lowering.
fn test_check_reports_an_instance_asked_for_in_a_match_arm() {
	root := os.join_path(os.vtmp_dir(), 'check_instance_match_arm_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "struct NoName {}\n\nfn show[T](x T) string {\n\treturn x.name\n}\n\nfn pick(a int) string {\n\treturn match a {\n\t\t1 { show(NoName{}) }\n\t\telse { '' }\n\t}\n}\n\nfn main() {\n\tprintln(pick(1))\n}\n")!
	result := os.exec([vexe, '-check', source])
	assert result.exit_code != 0
	assert result.output.contains('has no field named `name`'), result.output
}
