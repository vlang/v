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
	guarded := os.join_path(root, 'guarded.c')
	script := os.join_path(@VEXEROOT, 'cmd', 'tools', 'macos_vc_compat.awk')
	// The second include is already guarded, as in newer snapshots. The nested
	// block checks that the filter keeps the entire failure path Linux-only.
	text := '#include <sys/prctl.h>\n#if defined(__linux__)\n#include <sys/prctl.h>\n#endif\nint main(void) {\n\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n\t\tif (1) {\n\t\t\treturn 1;\n\t\t}\n\t}\n\treturn 0;\n}\n'
	os.write_file(source, text)!
	filtered := os.execute('awk -f ${os.quoted_path(script)} ${os.quoted_path(source)} > ${os.quoted_path(guarded)}')
	assert filtered.exit_code == 0, filtered.output
	assert os.read_file(source)! == text

	// Use only preprocessing: the host C compiler can exercise both target
	// conditions without cross-compilation libraries or a Linux prctl header.
	os.mkdir_all(os.join_path(root, 'sys'))!
	os.write_file(os.join_path(root, 'sys', 'prctl.h'), '#define PR_SET_PDEATHSIG 1\n')!
	cc := os.getenv_opt('CC') or { 'cc' }
	for linux in [false, true] {
		flags := if linux { '-D__linux__ -U__ANDROID__' } else { '-U__linux__' }
		preprocessed := os.execute('${cc} -E -P ${flags} -I${os.quoted_path(root)} ${os.quoted_path(guarded)}')
		assert preprocessed.exit_code == 0, preprocessed.output
		assert preprocessed.output.contains('prctl(') == linux
		assert preprocessed.output.contains('return 1;') == linux
		assert preprocessed.output.contains('return 0;')
	}
	// A missing Linux header must also be harmless to a macOS bootstrap.
	os.rm(os.join_path(root, 'sys', 'prctl.h'))!
	checked := os.execute('${cc} -fsyntax-only -U__linux__ ${os.quoted_path(guarded)}')
	assert checked.exit_code == 0, checked.output

	// Do not let a changed snapshot format leave the rest of the file inside
	// a silently unterminated platform guard.
	os.write_file(source, '\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n')!
	malformed := os.execute('awk -f ${os.quoted_path(script)} ${os.quoted_path(source)}')
	assert malformed.exit_code != 0
	assert malformed.output.contains('Unterminated prctl block')
}

fn test_macos_vc_compat_make_selects_only_the_macos_build_copy() {
	$if windows {
		return
	}
	make := os.find_abs_path_of_executable('gmake') or {
		os.find_abs_path_of_executable('make') or { panic(err) }
	}
	for target in ['Darwin', 'Linux', 'FreeBSD'] {
		// Platform simulation must not derive the legacy macOS bootstrap from
		// another host's kernel version (for example Linux 6.x).
		result := os.execute('${os.quoted_path(make)} -n -C ${os.quoted_path(@VEXEROOT)} local=1 _SYS=${target} TCCARCH=amd64 LEGACY= VEXE=./v all')
		assert result.exit_code == 0, result.output
		assert result.output.contains('awk -f') == (target == 'Darwin')
		assert result.output.contains('-o v1 ./vc/v_macos.c') == (target == 'Darwin')
		assert result.output.contains('-o v1 ./vc/v.c') == (target != 'Darwin')
	}
}
