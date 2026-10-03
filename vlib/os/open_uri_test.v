import os

fn test_open_uri_passes_uri_as_literal_data() {
	root := os.join_path(os.vtmp_dir(), 'uri opener ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'opener.v')
	binary := os.join_path(root, if os.user_os() == 'windows' { 'opener.exe' } else { 'opener' })
	output := os.join_path(root, 'arguments.txt')
	marker := os.join_path(root, 'injected')
	os.write_file(source, "import os\nfn main() { os.write_file(os.args[1], os.args[2..].join('\\n'))! }\n")!
	build := os.exec([@VEXE, '-o', binary, source])
	assert build.exit_code == 0, build.output
	previous := os.getenv_opt('VOPEN_URI_CMD')
	os.setenv('VOPEN_URI_CMD', '${os.quoted_path(binary)} ${os.quoted_path(output)} --flag', true)
	defer {
		if value := previous {
			os.setenv('VOPEN_URI_CMD', value, true)
		} else {
			os.unsetenv('VOPEN_URI_CMD')
		}
	}
	uri := 'https://example.com/$(touch "${marker}")?a=one two&b="quoted"'
	os.open_uri(uri)!
	assert os.read_file(output)! == '--flag\n${uri}'
	assert !os.exists(marker)
}
