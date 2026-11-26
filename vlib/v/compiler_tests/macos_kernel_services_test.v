import os

fn test_macos_kernel_services_without_host_imports() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_kernel_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VMODROOT, 'vlib/builtin/testdata/macos_kernel_services')
	bridge := os.join_path(fixture, 'kernel_services_oracle.c')
	for prod in [false, true] {
		output := os.join_path(root, 'standalone_${prod}')
		mut command := ['xcrun', 'clang', '-arch', 'x86_64', '-std=c11', '-ffreestanding',
			'-fno-builtin', '-fno-stack-protector', '-nostdlib', '-static', '-Wl,-e,_start',
			'-Wl,-no_pie', '-Wall', '-Wextra', '-Werror']
		if prod { command << '-O2' }
		command << [bridge, os.join_path(fixture, 'probe.c'), os.join_path(fixture, 'standalone.c'),
			os.join_path(fixture, 'standalone.s'), '-o', output]
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		undefined := os.exec(['xcrun', 'nm', '-u', output])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
		commands := os.exec(['xcrun', 'otool', '-l', output])
		assert commands.exit_code == 0, commands.output
		assert commands.output.contains('LC_UNIXTHREAD'), commands.output
		assert !commands.output.contains('LC_LOAD_DYLIB')
		assert !commands.output.contains('LC_LOAD_DYLINKER')
		ran := os.exec([output])
		assert ran.exit_code == 0, 'standalone exit ${ran.exit_code}: ${ran.output}'
	}
	output := os.join_path(root, 'v_bridge')
	built := os.exec([@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc', 'clang',
		'-o', output, os.join_path(fixture, 'main.c.v')])
	assert built.exit_code == 0, built.output
	ran := os.exec([output])
	assert ran.exit_code == 0 && ran.output.trim_space() == 'macOS kernel services ok', ran.output
}
