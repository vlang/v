import os

fn test_macos_vc_compat_preserves_linux_prctl_and_skips_it_elsewhere() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_vc_compat_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'snapshot.c')
	script := os.join_path(@VEXEROOT, 'cmd', 'tools', 'macos_vc_compat.sh')
	// The second include is already guarded, as in newer snapshots. The nested
	// block checks that the filter keeps the entire failure path Linux-only.
	text := '#include <sys/prctl.h>\n#if defined(__linux__)\n#include <sys/prctl.h>\n#endif\nint main(void) {\n\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n\t\tif (1) {\n\t\t\treturn 1;\n\t\t}\n\t}\n\treturn 0;\n}\n'
	os.write_file(source, text)!

	// Use only preprocessing: the host C compiler can exercise both target
	// conditions without cross-compilation libraries or a Linux prctl header.
	os.mkdir_all(os.join_path(root, 'sys'))!
	os.write_file(os.join_path(root, 'sys', 'prctl.h'), '#define PR_SET_PDEATHSIG 1\n')!
	cc := os.getenv_opt('CC') or { 'cc' }
	for linux in [false, true] {
		flags := if linux { '-D__linux__ -U__ANDROID__' } else { '-U__linux__' }
		preprocessed := os.exec(['bash', '${script}', source, cc, '-E', '-P',
			...(os.split_args(flags) or { panic(err) }), '-I' + '${root}'])
		assert preprocessed.exit_code == 0, preprocessed.output
		assert preprocessed.output.contains('prctl(') == linux
		assert preprocessed.output.contains('return 1;') == linux
		assert preprocessed.output.contains('return 0;')
	}
	// A missing Linux header must also be harmless to a macOS bootstrap.
	os.rm(os.join_path(root, 'sys', 'prctl.h'))!
	checked := os.exec(['bash', '${script}', source, cc, '-fsyntax-only', '-U__linux__'])
	assert checked.exit_code == 0, checked.output
	assert os.read_file(source)! == text
	assert os.ls(root)!.sorted() == ['snapshot.c', 'sys']

	// Preserve compiler argv, including spaces, and report compiler failures.
	compiler := os.join_path(root, 'compiler fixture.sh')
	args := os.join_path(root, 'args.txt')
	os.write_file(compiler, '#!/bin/sh\nstatus=$1\nshift\noutput=$1\nshift\nprintf \'%s\\n\' "$@" > "$output"\ncat >/dev/null\nexit "$status"\n')!
	command := 'bash ${os.quoted_path(script)} ${os.quoted_path(source)} sh ${os.quoted_path(compiler)}'
	failed := os.exec([...(os.split_args(command) or { panic(err) }), '37', '${args}',
		'flag with spaces'])
	assert failed.exit_code == 37, failed.output
	assert os.read_file(args)! == 'flag with spaces\n-x\nc\n-\n'

	// Do not let a changed snapshot format leave the rest of the file inside
	// a silently unterminated platform guard.
	os.write_file(source, '\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n')!
	malformed := os.exec([...(os.split_args(command) or { panic(err) }), '0', '${args}'])
	assert malformed.exit_code != 0
	assert malformed.output.contains('Unterminated prctl block')
}

fn test_macos_vc_compat_make_uses_the_single_snapshot() {
	$if windows {
		return
	}
	make := os.find_abs_path_of_executable('gmake') or {
		os.find_abs_path_of_executable('make') or { panic(err) }
	}
	for target in ['Darwin', 'Linux', 'FreeBSD'] {
		// Platform simulation must not derive the legacy macOS bootstrap from
		// another host's kernel version (for example Linux 6.x).
		result := os.exec(['${make}', '-n', '-C', @VEXEROOT, 'local=1', '_SYS=' + '${target}',
			'TCCARCH=amd64', 'LEGACY=', 'VEXE=./v', 'all'])
		assert result.exit_code == 0, result.output
		assert result.output.contains('macos_vc_compat.sh" "./vc/v.c"') == (target == 'Darwin')
		assert result.output.contains('-o v1 ./vc/v.c') == (target != 'Darwin')
		assert !result.output.contains('v_macos.c')
	}
}
