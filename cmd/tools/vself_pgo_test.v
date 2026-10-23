import os

fn pgo_test_run(root string, tool string, extra []string, fail bool, jobs string, flags string) os.Result {
	return os.exec(['env', '-u', 'CC', 'VFLAGS=${flags}', 'VOSARGS=', 'VJOBS=${jobs}',
		'LLVM_PROFILE_FILE=${root}/caller.profraw', 'PATH=${root}/bin:${os.getenv('PATH')}',
		'VEXE=${root}/v', 'VSELF_PGO_TEST_TRAINER=${root}/trainer',
		'VSELF_PGO_TEST_RESULT=${root}/result_source', 'VSELF_PGO_TEST_LOG=${root}/calls',
		'VSELF_PGO_TEST_FAIL=${int(fail)}', tool, 'self', '-prod', '-cc', '${root}/bin/clang',
		'-o', '${root}/result', ...extra])
}

fn test_production_self_build_profiles_cold_serial_and_parallel_work_and_falls_back() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vself pgo_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bin'))!
	defer { os.rmdir_all(root) or {} }
	// An old workspace must not be removed when a new self-build starts.
	os.mkdir(os.join_path(root, '.vself_pgo'))!
	os.write_file(os.join_path(root, '.vself_pgo', 'keep'), 'keep')!
	caller_profile := os.join_path(root, 'caller.profraw')
	os.write_file(caller_profile, 'caller profile')!
	scripts := {
		'v':                 r'#!/bin/sh
printf "BUILD %s\n" "$*" >> "$VSELF_PGO_TEST_LOG"
previous=
output=
for argument in "$@"; do
    if [ "$previous" = "-o" ]; then output="$argument"; fi
    previous="$argument"
done
if [ -z "$output" ]; then exit 2; fi
case "$*" in
    *-fprofile-generate=*) cp "$VSELF_PGO_TEST_TRAINER" "$output" ;;
    *) cp "$VSELF_PGO_TEST_RESULT" "$output" ;;
esac
chmod 755 "$output"
'
		'trainer':           r'#!/bin/sh
printf "TRAIN jobs=%s args=%s\n" "$VJOBS" "$*" >> "$VSELF_PGO_TEST_LOG"
if [ -n "${VFLAGS-}" ] || [ -n "${VEXE-}" ]; then exit 3; fi
if [ -n "${LLVM_PROFILE_FILE-}" ]; then exit 6; fi
if [ "$V_MACOS_V3_NO_FALLBACK" != "1" ]; then exit 4; fi
if [ "$VSELF_PGO_TEST_FAIL" = "1" ]; then exit 5; fi
printf "profile\n" > "$(dirname "$0")/profile/training-$VJOBS.profraw"
'
		'bin/clang':         '#!/bin/sh\nprintf "clang version mock\\n"\n'
		'bin/gcc':           '#!/bin/sh\nprintf "gcc (GCC) mock\\n"\n'
		'bin/llvm-profdata': r'#!/bin/sh
printf "MERGE %s\n" "$*" >> "$VSELF_PGO_TEST_LOG"
for argument in "$@"; do
    case "$argument" in
        -output=*) printf "merged\n" > "${argument#-output=}" ;;
    esac
done
'
		'result_source':     '#!/bin/sh\nexit 0\n'
	}
	for name, source in scripts {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		os.chmod(path, 0o755)!
	}
	tool := os.join_path(root, 'vself')
	build := os.exec([@VEXE, '-nocache', '-o', tool,
		os.join_path(os.dir(@VEXE), 'cmd', 'tools', 'vself.v')])
	assert build.exit_code == 0, build.output

	result := pgo_test_run(root, tool, [], false, '2', '')
	assert result.exit_code == 0, result.output
	calls := os.read_file(os.join_path(root, 'calls'))!
	lines := calls.split_into_lines()
	assert lines.filter(it.starts_with('BUILD ')).len == 2, calls
	assert calls.contains('-fprofile-generate='), calls
	assert calls.contains('-fprofile-use='), calls
	assert calls.contains("-cflags '-fprofile-generate=${root}/.vself_pgo_"), calls
	assert calls.contains("-cflags '-fprofile-use=${root}/.vself_pgo_"), calls
	build_calls := lines.filter(it.starts_with('BUILD '))
	assert build_calls[0].contains('-fprofile-update=atomic'), calls
	assert !build_calls[1].contains('-fprofile-update=atomic'), calls
	assert lines.filter(it.starts_with('TRAIN ')).len == 2, calls
	assert calls.contains('TRAIN jobs=2 args=-nocache '), calls
	assert calls.contains('TRAIN jobs=1 args=-nocache '), calls
	assert calls.contains('-building-v '), calls
	assert calls.contains('-compile-backend fastc '), calls
	assert lines.filter(it.starts_with('MERGE ')).len == 1, calls
	assert os.is_executable(os.join_path(root, 'result'))
	assert os.read_file(os.join_path(root, '.vself_pgo', 'keep'))! == 'keep'
	assert os.ls(root)!.filter(it.starts_with('.vself_pgo_')).len == 0
	assert os.read_file(caller_profile)! == 'caller profile'

	// An LLVM utility in PATH can belong to an older, incompatible compiler.
	// Both an explicit compiler path and its symlink must select the sibling tool.
	compiler_dir := os.join_path(root, 'compiler')
	os.mkdir(compiler_dir)!
	os.cp(os.join_path(root, 'bin', 'clang'), os.join_path(compiler_dir, 'clang'))!
	os.chmod(os.join_path(compiler_dir, 'clang'), 0o755)!
	os.mv(os.join_path(root, 'bin', 'llvm-profdata'), os.join_path(compiler_dir, 'llvm-profdata'))!
	os.write_file(os.join_path(root, 'bin', 'llvm-profdata'), '#!/bin/sh\nexit 55\n')!
	os.chmod(os.join_path(root, 'bin', 'llvm-profdata'), 0o755)!
	compiler_link := os.join_path(root, 'bin', 'clang-linked')
	os.symlink(os.join_path(compiler_dir, 'clang'), compiler_link)!
	compiler_launcher := os.join_path(root, 'bin', 'clang-launcher')
	os.write_file(compiler_launcher, '#!/bin/sh\nprintf "%s\\n" "clang version mock" ' +
		os.quoted_path('InstalledDir: ${compiler_dir}') + '\n')!
	os.chmod(compiler_launcher, 0o755)!
	for compiler in [os.join_path(compiler_dir, 'clang'), compiler_link, compiler_launcher] {
		for options in [['-cc', compiler], ['-cc=${compiler}']] {
			os.write_file(os.join_path(root, 'calls'), '')!
			selected := pgo_test_run(root, tool, options, false, '2', '')
			assert selected.exit_code == 0, selected.output
			assert !selected.output.contains('falling back'), selected.output
			selected_calls := os.read_file(os.join_path(root, 'calls'))!.split_into_lines()
			assert selected_calls.filter(it.starts_with('MERGE ')).len == 1, selected_calls.join('\n')
			assert selected_calls.filter(it.starts_with('BUILD ')).len == 2, selected_calls.join('\n')
		}
	}
	os.rm(os.join_path(root, 'bin', 'llvm-profdata'))!
	os.mv(os.join_path(compiler_dir, 'llvm-profdata'), os.join_path(root, 'bin', 'llvm-profdata'))!
	// A compiler without a sibling utility retains the PATH fallback.
	os.write_file(os.join_path(root, 'calls'), '')!
	path_fallback := pgo_test_run(root, tool, ['-cc', os.join_path(compiler_dir, 'clang')],
		false, '2', '')
	assert path_fallback.exit_code == 0, path_fallback.output
	assert !path_fallback.output.contains('falling back'), path_fallback.output

	for extra in [['-parallel-cc'], ['-no-prod-options'], ['-cflags', '-fprofile-use=external']] {
		os.write_file(os.join_path(root, 'calls'), '')!
		plain := pgo_test_run(root, tool, extra, false, '2', '')
		assert plain.exit_code == 0, plain.output
		plain_calls := os.read_file(os.join_path(root, 'calls'))!
		assert plain_calls.split_into_lines().filter(it.starts_with('BUILD ')).len == 1, plain_calls
		assert !plain_calls.contains('-fprofile-generate='), plain_calls
		assert !plain_calls.contains('TRAIN '), plain_calls
	}
	for compiler in ['clang', 'gcc'] {
		other := if compiler == 'clang' { 'gcc' } else { 'clang' }
		flags := '-cc ' + os.quoted_path(os.join_path(root, 'bin', other))
		extra := if compiler == 'clang' { []string{} } else { ['-cc', '${root}/bin/gcc'] }
		for case_index, options in [extra,
			['-cc', '${root}/bin/gcc', '-cc', '${root}/bin/${compiler}'],
			['-cc', '${root}/bin/${other}', '-cc=${root}/bin/${compiler}'],
			['-cc=${root}/bin/${other}', '-cc', '${root}/bin/${compiler}'],
			['-profile', '-cc', '${root}/bin/${compiler}']] {
			os.write_file(os.join_path(root, 'calls'), '')!
			inherited_flags := if case_index == 0 { flags } else { '' }
			selected := pgo_test_run(root, tool, options, false, '2', inherited_flags)
			assert selected.exit_code == 0, selected.output
			selected_calls := os.read_file(os.join_path(root, 'calls'))!
			expected_builds := if compiler == 'clang' { 2 } else { 1 }
			assert selected_calls.split_into_lines().filter(it.starts_with('BUILD ')).len == expected_builds, selected_calls
			assert selected_calls.contains('TRAIN ') == (compiler == 'clang'), selected_calls
		}
	}
	for extra in [([]string{}), ['-no-parallel']] {
		os.write_file(os.join_path(root, 'calls'), '')!
		serial := pgo_test_run(root, tool, extra, false, if extra.len == 0 { '1' } else { '2' }, '')
		assert serial.exit_code == 0, serial.output
		serial_calls := os.read_file(os.join_path(root, 'calls'))!
		assert serial_calls.split_into_lines().filter(it.starts_with('TRAIN ')).len == 1, serial_calls
	}
	os.write_file(os.join_path(root, 'calls'), '')!
	fallback := pgo_test_run(root, tool, [], true, '2', '')
	assert fallback.exit_code == 0, fallback.output
	assert fallback.output.contains('falling back to a regular self-build'), fallback.output
	fallback_calls := os.read_file(os.join_path(root, 'calls'))!
	assert fallback_calls.split_into_lines().filter(it.starts_with('BUILD ')).len == 2, fallback_calls
	assert !fallback_calls.contains('MERGE '), fallback_calls
	assert os.ls(root)!.filter(it.starts_with('.vself_pgo_')).len == 0
	assert os.read_file(caller_profile)! == 'caller profile'
}
