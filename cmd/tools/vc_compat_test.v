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
		preprocessed := os.exec(['sh', '${script}', source, cc, '-E', '-P',
			...(os.split_args(flags) or { panic(err) }), '-I' + '${root}'])
		assert preprocessed.exit_code == 0, preprocessed.output
		assert preprocessed.output.contains('prctl(') == linux
		assert preprocessed.output.contains('return 1;') == linux
		assert preprocessed.output.contains('return 0;')
	}
	// A missing Linux header must also be harmless to a macOS bootstrap.
	os.rm(os.join_path(root, 'sys', 'prctl.h'))!
	checked := os.exec(['sh', '${script}', source, cc, '-fsyntax-only', '-U__linux__'])
	assert checked.exit_code == 0, checked.output
	assert os.read_file(source)! == text
	assert os.ls(root)!.sorted() == ['snapshot.c', 'sys']

	// Preserve compiler argv, including spaces, and report compiler failures.
	compiler := os.join_path(root, 'compiler fixture.sh')
	args := os.join_path(root, 'args.txt')
	os.write_file(compiler, '#!/bin/sh\nstatus=$1\nshift\noutput=$1\nshift\nprintf \'%s\\n\' "$@" > "$output"\ncat >/dev/null\nexit "$status"\n')!
	command := 'sh ${os.quoted_path(script)} ${os.quoted_path(source)} sh ${os.quoted_path(compiler)}'
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
		assert result.output.contains('\nsh "./cmd/tools/vc_compat.sh" "./vc/v.c"'), result.output
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
			checked := os.exec(['sh', script, source, cc, '-std=c99', '-fsyntax-only'])
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
	checked := os.exec(['sh', script, source, cc, '-std=c99', '-fsyntax-only'])
	assert checked.exit_code == 0, checked.output
	// Bootstrap snapshots may contain string literals with non-UTF-8 bytes.
	bytes := [u8(0xff), u8(0xfe)].bytestr()
	os.write_file(source, current + 'const char *binary = "${bytes}";\n')!
	binary_checked := os.exec(['env', 'LC_ALL=en_US.UTF-8', 'sh', script, source, cc, '-std=c99',
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
	assert result.output.contains('sh cmd/tools/vc_compat.sh vc/v.c'), result.output
	assert !result.output.contains('bash cmd/tools/vc_compat.sh'), result.output
}

fn test_vc_compat_runs_with_a_posix_shell_and_no_bash_in_path() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vc_compat_posix_${os.getpid()}')
	bin := os.join_path(root, 'bin')
	tmp := os.join_path(root, 'tmp')
	os.mkdir_all(bin)!
	os.mkdir_all(tmp)!
	defer { os.rmdir_all(root) or {} }
	// Prefer dash so Bash extensions are rejected even on a host whose sh is Bash.
	shell := os.find_abs_path_of_executable('dash') or {
		os.find_abs_path_of_executable('sh')!
	}
	os.symlink(shell, os.join_path(bin, 'sh'))!
	for utility in ['awk', 'dirname', 'mktemp', 'mkfifo', 'rm', 'cat'] {
		path := os.find_abs_path_of_executable(utility)!
		os.symlink(path, os.join_path(bin, utility))!
	}
	source := os.join_path(root, 'snapshot with spaces.c')
	text := 'V_PEAK_RSS_SCOPE long long v3_bench_peak_rss_kb(void) { return 42; }\n'
	os.write_file(source, text)!
	compiler := os.join_path(root, 'compiler with spaces.sh')
	os.write_file(compiler, '#!/bin/sh\noutput=$1\nshift\nprintf \'%s\\n\' "$@" > "$output.args"\ncat > "$output"\n')!
	output := os.join_path(root, 'filtered.c')
	command := ['env', 'PATH=${bin}', 'TMPDIR=${tmp}', 'sh',
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh'), source, 'sh', compiler, output,
		'--', '-lfixture', 'link input with spaces']
	checked := os.exec(command)
	assert checked.exit_code == 0, checked.output
	assert os.read_file(output)! == text.replace('long long', 'i64')
	assert os.read_file('${output}.args')! == '-x\nc\n-\n-x\nnone\n-lfixture\nlink input with spaces\n'
	assert os.ls(tmp)!.len == 0
	// A successful consumer must not hide a failed filter.
	os.write_file(source, '\tif (prctl(PR_SET_PDEATHSIG, 0, 0, 0, 0) != 0) {\n')!
	malformed := os.exec(command)
	assert malformed.exit_code != 0
	assert malformed.output.contains('Unterminated prctl block'), malformed.output
	assert os.ls(tmp)!.len == 0
	// A compiler that exits before reading stdin must not strand the filter.
	os.write_file(source, text.repeat(1000))!
	os.write_file(compiler, '#!/bin/sh\nexit 37\n')!
	failed := os.exec(command)
	assert failed.exit_code == 37, failed.output
	assert os.ls(tmp)!.len == 0
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
	checked := os.exec(['sh', os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh'), source,
		cc, '-std=c99', '-fno-builtin', '-o', executable, '--', '-lm'])
	assert checked.exit_code == 0, checked.output
	ran := os.exec([executable])
	assert ran.exit_code == 0, ran.output
}

fn test_vc_compat_bridges_only_snapshots_without_native_uint() ! {
	root := os.join_path(os.vtmp_dir(), 'vc_compat_uint_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'snapshot with spaces.c')
	output := os.join_path(root, 'filtered.c')
	// C qualifier metadata already mentioned uint in old snapshots. It does
	// not mean the compiler recognizes it as a V primitive.
	legacy := 'u32 flat__source_type_usize;\nu32 types__qualify_source_type_uint;\n_SL(name, "usize", 5);\nconst string casts[] = {(string){"usize", 5, 1}};\nu8* malloc_noscan(ptrdiff_t n);\n_SL(alloc, "u8* malloc_noscan(ptrdiff_t n);", 31);\n_SL(copy, "void* memdup(void* src, ptrdiff_t sz);", 38);\n_SL(align, "void* memdup_align(void* src, ptrdiff_t sz, ptrdiff_t alignment);", 65);\nc__FlatGen__writeln(g, alloc);\nc__FlatGen__writeln(g, copy);\nc__FlatGen__writeln(g, align);\nconst char* embedded = "\\\"usize\\\", 5";\n'
	bytes := [u8(0xff), u8(0xfe)].bytestr()
	for declarations in ['', 'u32 flat__source_type_uint;\n'] {
		original := declarations + legacy + 'const char* binary = "${bytes}";\n'
		os.write_file(source, original)!
		mut filtered := ''
		$if windows {
			checked := os.exec(['powershell', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
				os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat_uint.ps1'), '-Source', source,
				'-Destination', output])
			assert checked.exit_code == 0, checked.output
			filtered = os.read_file(output)!
		} $else {
			_ = output
			checked := os.exec(['env', 'LC_ALL=C', 'awk', '-f',
				os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.awk'), source])
			assert checked.exit_code == 0, checked.output
			filtered = checked.output
		}
		expected := if declarations == '' {
			original.replace('"usize", 5', '"uint", 4')
				.replace('c__FlatGen__writeln(g, alloc);', 'c__FlatGen__writeln(g, string__replace(alloc, (string){(u8*)"ptrdiff_t", 9, 1}, g->int_ct));')
				.replace('c__FlatGen__writeln(g, copy);', 'c__FlatGen__writeln(g, string__replace(copy, (string){(u8*)"ptrdiff_t", 9, 1}, g->int_ct));')
				.replace('c__FlatGen__writeln(g, align);', 'c__FlatGen__writeln(g, string__replace(align, (string){(u8*)"ptrdiff_t", 9, 1}, g->int_ct));')
		} else {
			original
		}
		assert filtered == expected
		assert os.read_file(source)! == original
	}
}

fn test_vc_compat_relocates_native_includes_without_expanding_headers() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vc_compat_includes_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'snapshot.c')
	output := os.join_path(root, 'stream.c')
	compiler := os.join_path(root, 'compiler.sh')
	os.write_file(compiler, '#!/bin/sh\ncat > "$1"\n')!
	text := '#include "/home/runner/work/v/v/vlib/builtin/track_heap_checks.h"\n#include "C:\\runner\\v\\thirdparty\\stdatomic\\win\\atomic.h"\n#include "/opt/sdk/vendor.h"\n#include "local.h"\nconst char *literal = "vlib/builtin/track_heap_checks.h";\n'
	os.write_file(source, text)!
	script := os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat.sh')
	filtered := os.exec(['sh', script, source, 'sh', compiler, output])
	assert filtered.exit_code == 0, filtered.output
	stream := os.read_file(output)!
	vroot := os.real_path(@VEXEROOT).replace('\\', '/')
	assert stream.contains('#include "${vroot}/vlib/builtin/track_heap_checks.h"')
	assert stream.contains('#include "${vroot}/thirdparty/stdatomic/win/atomic.h"')
	assert stream.contains('#include "/opt/sdk/vendor.h"\n#include "local.h"')
	assert stream.contains('const char *literal = "vlib/builtin/track_heap_checks.h";')
	assert os.read_file(source)! == text
}

fn test_windows_vc_compat_relocates_includes_and_preserves_snapshot_bytes() ! {
	powershell := $if windows {
		os.find_abs_path_of_executable('powershell')!
	} $else {
		os.find_abs_path_of_executable('pwsh') or { return }
	}
	root := os.join_path(os.real_path(os.vtmp_dir()), 'vc_compat_windows_includes_${os.getpid()}')
	checkout := os.join_path(root, 'checkout λ with spaces')
	tools := os.join_path(checkout, 'cmd', 'tools')
	os.mkdir_all(tools)!
	defer { os.rmdir_all(root) or {} }
	script := os.join_path(tools, 'vc_compat_uint.ps1')
	os.cp(os.join_path(@VEXEROOT, 'cmd', 'tools', 'vc_compat_uint.ps1'), script)!
	source := os.join_path(root, 'snapshot.c')
	output := os.join_path(root, 'filtered.c')
	unix_header := '/generator/v/vlib/builtin/core_windows.h'
	windows_header := r'C:\runner\v\thirdparty\stdatomic\win\atomic.h'
	cmd_header := '/generator/v/cmd/tools/helper.h'
	binary := [u8(0xff), u8(0xfe)].bytestr()
	text := '#include "${unix_header}" // keep this comment\n#include "${windows_header}"\n#include "${cmd_header}"\n#include "/opt/sdk/vendor.h"\n#include "local.h"\n#include <stdio.h>\nconst char *literal = "vlib/builtin/core_windows.h";\nconst char *binary = "${binary}";\n'
	os.write_file(source, text)!
	filtered := os.exec([powershell, '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File', script,
		'-Source', source, '-Destination', output])
	assert filtered.exit_code == 0, filtered.output
	vroot := os.real_path(checkout).replace('\\', '/')
	expected := text.replace(unix_header, '${vroot}/vlib/builtin/core_windows.h')
		.replace(windows_header, '${vroot}/thirdparty/stdatomic/win/atomic.h')
		.replace(cmd_header, '${vroot}/cmd/tools/helper.h')
	assert os.read_file(output)! == expected
	assert os.read_file(source)! == text
}
