import os

fn test_macos_shared_object_and_hosted_threads_keep_the_hosts_tls() ! {
	root := os.join_path(os.vtmp_dir(), 'macos_thread_selection_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn twice(value int) int { return value * 2 }\nfn main() { task := spawn twice(21); println(task.wait()) }\n')!
	for role in ['hosted', 'shared', 'object', 'explicit_host'] {
		output := os.join_path(root, '${role}.c')
		mut command := [@VEXE, '-no-retry-compilation', '-nocache', '-gc', 'none', '-os', 'macos',
			'-arch', 'amd64', '-enable-globals']
		match role {
			'shared' { command << ['-freestanding', '-shared'] }
			'object' { command << ['-freestanding', '-is_o'] }
			'explicit_host' { command << ['-freestanding', '-d', 'v_bare_no_startup'] }
			else {}
		}
		command << ['-o', output, source]
		built := os.exec(command)
		assert built.exit_code == 0, '${role}: ${built.output}'
		code := os.read_file(output)!
		assert code.contains('pthread_create(&result.handle,'), role
		assert !code.contains('v_bare_thread_create(start, arg,'), role
		assert !code.contains('bare_macos_tls_activate('), role
		assert !code.contains('__tlv_bootstrap:'), role
		assert !code.contains('%gs:'), role
	}
}
