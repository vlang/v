import os

const uri_opener_probe = '--vtest-uri-opener'

fn testsuite_begin() {
	if os.args.len >= 4 && os.args[1] == uri_opener_probe {
		os.write_file(os.args[2], os.args[3..].join('\n'))!
		exit(0)
	}
}

fn test_open_uri_passes_uri_as_literal_data() {
	root := os.join_path(os.vtmp_dir(), 'uri opener ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	binary := os.join_path(root, if os.user_os() == 'windows' { 'opener.exe' } else { 'opener' })
	output := os.join_path(root, 'arguments.txt')
	marker := os.join_path(root, 'injected')
	os.cp(os.executable(), binary)!
	previous := os.getenv_opt('VOPEN_URI_CMD')
	os.setenv('VOPEN_URI_CMD', '${os.quoted_path(binary)} ${uri_opener_probe} ${os.quoted_path(output)} --flag', true)
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
