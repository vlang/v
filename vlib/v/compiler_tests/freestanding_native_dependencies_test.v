import os
import v.cmdexec
import v.pref

fn test_static_package_private_dependencies_and_archive_cycles_without_link_flags() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	os.find_abs_path_of_executable('pkg-config') or { return }
	root := os.join_path(os.vtmp_dir(), 'vlibc_native_dependencies_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	prefix := os.join_path(root, 'prefix with spaces')
	lib := os.join_path(prefix, 'lib')
	include := os.join_path(prefix, 'include')
	pcdir := os.join_path(lib, 'pkgconfig')
	os.mkdir_all(pcdir)!
	os.mkdir_all(include)!
	os.write_file(os.join_path(prefix, 'vlibc-target'), 'vlibc-native-v1\nlinux\n${pref.host_arch()}\n')!
	os.write_file(os.join_path(include, 'fixture_dep.h'), 'int declared_value(void);\n')!
	for name, source in {
		'entry':  'int dependency_middle(void); int declared_value(void) { return dependency_middle()+1; }'
		'middle': 'int trailing(void); int dependency_middle(void) { return trailing()+1; }'
		'trail':  'int trailing(void) { return 40; }'
	} {
		file := os.join_path(root, '${name}.c')
		os.write_file(file, source)!
		built := cmdexec.run('cc', ['-c', file, '-o', os.join_path(root, '${name}.o')])
		assert built.exit_code == 0, built.output
	}
	for name, objects in {
		'alpha': ['entry', 'trail']
		'beta':  ['middle']
	} {
		mut args := ['rcs', os.join_path(lib, 'lib${name}.a')]
		for object in objects { args << os.join_path(root, '${object}.o') }
		archived := cmdexec.run('ar', args)
		assert archived.exit_code == 0, archived.output
	}
	os.write_file(os.join_path(pcdir, 'vlibc-fixture.pc'), 'prefix=${prefix}\nlibdir=${lib}\nincludedir=${include}\nName: vlibc-fixture\nDescription: archive cycle with a private dependency\nVersion: 1\nLibs: -L"\${libdir}" -lalpha\nLibs.private: -lbeta -lm -lpthread -ldl\nCflags: -I"\${includedir}"\n')!
	main := os.join_path(root, 'main.c.v')
	os.write_file(main, 'module main\n#pkgconfig vlibc-fixture\n#include <fixture_dep.h>\nfn C.declared_value() i32\nfn main() { assert C.declared_value() == 42 }\n')!
	previous := os.getenv_opt('VLIBC_PREFIX')
	defer {
		if value := previous {
			os.setenv('VLIBC_PREFIX', value, true)
		} else {
			os.unsetenv('VLIBC_PREFIX')
		}
	}
	os.setenv('VLIBC_PREFIX', prefix, true)
	for compiler in ['gcc', 'clang'] {
		os.find_abs_path_of_executable(compiler) or { continue }
		binary := os.join_path(root, compiler)
		flags := '${binary}.flags'
		built := cmdexec.run(@VEXE, ['-syscalls', '-vlibc', '-no-system-libc', '-ldflags', '-static',
			'-cc', compiler, '-dump-c-flags', flags, '-o', binary, main])
		assert built.exit_code == 0, built.output
		metadata := os.read_file(flags)!
		for dependency in ['-lalpha', '-lbeta', '-lm', '-lpthread', '-ldl'] {
			assert metadata.contains(dependency), metadata
		}
		assert cmdexec.run(binary, []).exit_code == 0
		undefined := cmdexec.run('nm', ['-u', binary])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
		elf := cmdexec.run('readelf', ['-l', binary])
		assert elf.exit_code == 0 && !elf.output.contains('INTERP') && !elf.output.contains('DYNAMIC'), elf.output
	}
	missing := os.join_path(root, 'missing.v')
	os.write_file(missing, '#flag -lvlibc_intentionally_missing_48617\nfn main() {}\n')!
	failed := cmdexec.run(@VEXE, ['-syscalls', '-d', 'vlibc', '-o', os.join_path(root, 'missing'),
		missing])
	assert failed.exit_code != 0 && failed.output.contains('vlibc_intentionally_missing_48617'), failed.output
	assert !failed.output.contains('retrying with') && !failed.output.contains('Unknown argument'), failed.output
}

fn test_freestanding_literal_main_keeps_exported_runtime_dependencies() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'freestanding_literal_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'hello.v')
	os.write_file(source, "fn main() { println('hello') }")!
	for cc in ['gcc', 'clang'] {
		os.find_abs_path_of_executable(cc) or { continue }
		output := os.join_path(dir, cc)
		built := cmdexec.run(@VEXE, ['-new-compiler', '-cc', cc, '-freestanding', '-ldflags', '-static',
			'-o', output, source])
		assert built.exit_code == 0, built.output
		executed := cmdexec.run(output, [])
		assert executed.exit_code == 0 && executed.output == 'hello\n', executed.output
	}
}

fn test_generated_shim_project_links_target_runtime_and_loader() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	os.find_abs_path_of_executable('gcc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'shim_c_project_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'worker.v')
	project := os.join_path(dir, 'project')
	os.write_file(source, "fn worker() int { return 42 }\nfn main() { task := spawn worker()\nassert task.wait() == 42\nprintln('worker ok') }")!
	built := cmdexec.run(@VEXE, ['-cc', 'gcc', '-gc', 'none', '-shim-libc', '-generate-c-project',
		project, source])
	assert built.exit_code == 0, built.output
	linked := cmdexec.run('sh', [os.join_path(project, 'build.sh')])
	assert linked.exit_code == 0, linked.output
	executed := cmdexec.run(os.join_path(project, 'worker'), [])
	assert executed.exit_code == 0 && executed.output == 'worker ok\n', executed.output
}
