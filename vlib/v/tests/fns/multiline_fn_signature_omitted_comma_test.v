import os

fn test_multiline_fn_signature_can_omit_commas() {
	source_path := os.join_path(os.vtmp_dir(),
		'issue_22021_multiline_fn_signature_${os.getpid()}.v')
	source := "fn multiline_greet(\n\tsalutation string\n\tname string\n) string {\n\treturn 'Hey, ' + salutation + ' ' + name + '!'\n}\n\nfn main() {\n\tassert multiline_greet(\n\t\t'Mr.'\n\t\t'Joe'\n\t) == 'Hey, Mr. Joe!'\n\tgreeter := fn (salutation string\n\t\tname string) string {\n\t\treturn 'Hello, ' + salutation + ' ' + name + '!'\n\t}\n\tassert greeter('Ms.', 'Jane') == 'Hello, Ms. Jane!'\n}\n"
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
	}
	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
}
