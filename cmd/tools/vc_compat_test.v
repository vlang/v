import os

fn test_vc_compat_preserves_linux_prctl_and_skips_it_elsewhere() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vc_compat_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'snapshot.c')
	script := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh')
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
	assert os.read_file(args)! == 'flag with spaces\n-x\nc\n-\n-x\nnone\n'

	// Do not let a changed snapshot format leave the rest of the file inside
	// a silently unterminated platform guard.
	os.write_file(source, '\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n')!
	malformed := os.exec([...(os.split_args(command) or { panic(err) }), '0', '${args}'])
	assert malformed.exit_code != 0
	assert malformed.output.contains('Unterminated prctl block')
}

fn test_vc_compat_make_uses_the_single_snapshot() {
	$if windows {
		return
	}
	make := os.find_abs_path_of_executable('gmake') or {
		os.find_abs_path_of_executable('make') or { panic(err) }
	}
	for target in ['Darwin', 'Linux', 'FreeBSD', 'OpenBSD'] {
		// Platform simulation must not derive the legacy macOS bootstrap from
		// another host's kernel version (for example Linux 6.x).
		result := os.exec(['${make}', '-n', '-C', @VEXEROOT, 'local=1', '_SYS=' + '${target}',
			'TCCARCH=amd64', 'LEGACY=', 'VEXE=./v', 'all'])
		assert result.exit_code == 0, result.output
		assert result.output.contains('vc_compat.sh" "./vc/v.c"'), result.output
		assert !result.output.contains('-o v1 ./vc/v.c'), result.output
		if target == 'OpenBSD' {
			assert result.output.contains('cmd/tools/openbsd_vc_compat.c'), result.output
		}
		assert !result.output.contains('v_macos.c')
	}
}

fn test_vc_compat_peak_rss_matches_i64_on_linux_and_macos() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vc_compat_peak_rss_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'snapshot with spaces.c')
	script := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh')
	cc := os.getenv_opt('CC') or { 'cc' }
	for spelling in ['long', 'long long'] {
		for scope in ['V_PEAK_RSS_SCOPE', 'static', 'static inline'] {
			text := 'typedef ${spelling} i64;\n#define V_PEAK_RSS_SCOPE\n${scope} long long v3_bench_peak_rss_kb(void) { return 42; }\ni64 v3_bench_peak_rss_kb(void);\n'
			os.write_file(source, text)!
			original := os.exec([cc, '-std=c99', '-fsyntax-only', source])
			assert (original.exit_code == 0) == (spelling == 'long long'), original.output
			checked := os.exec(['bash', script, source, cc, '-std=c99', '-fsyntax-only'])
			assert checked.exit_code == 0, checked.output
			assert os.read_file(source)! == text
		}
	}
	// Current snapshots and embedded source strings must pass through unchanged.
	current := 'typedef long i64;\n#define V_PEAK_RSS_INT i64\n#define V_PEAK_RSS_SCOPE\nV_PEAK_RSS_SCOPE V_PEAK_RSS_INT v3_bench_peak_rss_kb(void) { return 42; }\ni64 v3_bench_peak_rss_kb(void);\nconst char *embedded = "V_PEAK_RSS_SCOPE long long v3_bench_peak_rss_kb(void) {";\n'
	os.write_file(source, current)!
	filtered := os.exec(['awk', '-f', os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.awk'),
		source])
	assert filtered.exit_code == 0, filtered.output
	assert filtered.output == current
	checked := os.exec(['bash', script, source, cc, '-std=c99', '-fsyntax-only'])
	assert checked.exit_code == 0, checked.output
	// Bootstrap snapshots may contain string literals with non-UTF-8 bytes.
	bytes := [u8(0xff), u8(0xfe)].bytestr()
	os.write_file(source, current + 'const char *binary = "${bytes}";\n')!
	binary_checked := os.exec(['env', 'LC_ALL=en_US.UTF-8', 'bash', script, source, cc, '-std=c99',
		'-fsyntax-only'])
	assert binary_checked.exit_code == 0, binary_checked.output
}

fn test_vc_compat_portable_makefile_streams_the_snapshot() {
	$if windows {
		return
	}
	make := os.find_abs_path_of_executable('make') or { panic(err) }
	result := os.exec([make, '-f', 'Makefile', '-n', '-C', @VEXEROOT, 'v'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('bash cmd/tools/vc_compat.sh vc/v.c'), result.output
}

fn test_vc_compat_links_libraries_after_the_streamed_source() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vc_compat_link_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'snapshot.c')
	executable := os.join_path(root, 'snapshot')
	os.write_file(source, '#include <math.h>\nint main(void) { volatile double x = 1.0; return pow(x, 2.5) != 1.0; }\n')!
	cc := os.getenv_opt('CC') or { 'cc' }
	checked := os.exec(['bash', os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh'), source,
		cc, '-std=c99', '-fno-builtin', '-o', executable, '--', '-lm'])
	assert checked.exit_code == 0, checked.output
	ran := os.exec([executable])
	assert ran.exit_code == 0, ran.output
}
