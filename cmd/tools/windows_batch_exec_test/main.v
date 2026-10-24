module main

import os

struct BuildScript {
	cc     string
	script string
}

const compilers = ['gcc', 'msvc', 'tcc']

fn read_workflows(root string) map[string]string {
	mut m := map[string]string{}
	for cc in compilers {
		path := os.join_path(root, '.github', 'workflows', 'windows_ci_${cc}.yml')
		m[cc] = os.read_file(path) or { '' }
	}
	return m
}

fn extract_build_scripts(workflows map[string]string) []BuildScript {
	mut result := []BuildScript{}
	for cc, wf in workflows {
		lines := wf.split_into_lines()
		mut i := 0
		for i < lines.len {
			if lines[i].contains('run: |') {
				mut block := []string{}
				mut j := i + 1
				for j < lines.len {
					if lines[j].len == 0 || lines[j].starts_with('        ') {
						if lines[j].trim_space().len > 0 {
							mut line := lines[j]
							if line.starts_with('          ') { line = line[10..] }
							block << line
						} else {
							block << ''
						}
						j++
					} else {
						break
					}
				}
				script := block.join('\n') + '\n'
				if script.contains('.\\makev.bat -${cc}') {
					result << BuildScript{ cc: cc, script: script }
				}
				i = j
			} else {
				i++
			}
		}
	}
	return result
}

fn run_build(root string, script string, makev_exit int, symlink_exit int, stats_exit int) (int, []string) {
	calls := os.join_path(root, 'calls.txt')
	os.rm(calls) or {}
	os.setenv('VTEST_CI_CALLS', calls, true)
	os.setenv('VTEST_MAKEV_EXIT', makev_exit.str(), true)
	os.setenv('VTEST_SYMLINK_EXIT', symlink_exit.str(), true)
	os.setenv('VTEST_STATS_EXIT', stats_exit.str(), true)

	body := "\$ErrorActionPreference = 'Stop'\n" + script +
		"\nif (Test-Path -LiteralPath variable:\\LASTEXITCODE) { exit \$LASTEXITCODE }\n"
	path := os.join_path(root, 'build.ps1')
	os.write_file(path, body) or { return 1, []string{} }

	pwsh := os.find_abs_path_of_executable('pwsh') or {
		os.find_abs_path_of_executable('powershell') or { 'powershell' }
	}
	res := os.execute('${pwsh} -NoProfile -NonInteractive -File "${path}"')

	observed := if os.exists(calls) {
		os.read_file(calls) or { '' }.split_into_lines().filter(it.trim_space().len > 0)
	} else {
		[]string{}
	}
	return res.exit_code, observed
}

fn main() {
	$if !windows {
		println('BatchExecutionTests requires Windows; skipping')
		return
	}
	gcc := os.find_abs_path_of_executable('gcc') or {
		eprintln('requires gcc')
		return
	}
	pwsh := os.find_abs_path_of_executable('pwsh') or {
		eprintln('requires pwsh (PowerShell 7); not available in this environment')
		return
	}
	_ = pwsh

	root := os.join_path(os.temp_dir(), 'wbe_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { eprintln('mkdir failed'); exit(1) }
	defer { os.rmdir_all(root) or {} }

	source := os.join_path(root, 'probe.c')
	os.write_file(source, '#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\nint main(int argc, char **argv) {\n  FILE *calls = fopen(getenv("VTEST_CI_CALLS"), "a");\n  if (!calls) return 90;\n  for (int i = 1; i < argc; ++i) fprintf(calls, "%s%s", i == 1 ? "" : " ", argv[i]);\n  fputc(\'\\n\', calls);\n  fclose(calls);\n  const char *key = argc > 1 && strcmp(argv[1], "symlink") == 0 ? "VTEST_SYMLINK_EXIT" : "VTEST_STATS_EXIT";\n  const char *value = getenv(key);\n  return value ? atoi(value) : 0;\n}\n') or {}
	os.execute('${gcc} -std=c11 -Wall -Wextra -Werror -o "${root}/v.exe" "${source}"')

	os.write_file(os.join_path(root, 'makev.bat'), '@echo off\r\nexit /b %VTEST_MAKEV_EXIT%\r\n') or {}

	repo_root := os.join_path(@DIR, '..', '..', '..')
	workflows := read_workflows(repo_root)
	scripts := extract_build_scripts(workflows)
	if scripts.len == 0 {
		eprintln('no build scripts found')
		exit(1)
	}

	for s in scripts {
		code, calls := run_build(root, s.script, 47, 0, 0)
		assert code == 47, '${s.cc}: expected 47 got ${code}'
		assert calls.len == 0, '${s.cc}: expected no calls got ${calls}'
	}
	println('test_makev_failure_stops_before_a_working_v: OK')

	for s in scripts {
		code, calls := run_build(root, s.script, 0, 48, 0)
		assert code == 48, '${s.cc}: expected 48 got ${code}'
		assert calls.len == 1 && calls[0] == 'symlink', '${s.cc}: expected [symlink] got ${calls}'
	}
	println('test_symlink_failure_stops_before_later_commands: OK')

	for s in scripts {
		code, calls := run_build(root, s.script, 0, 0, 0)
		assert code == 0, '${s.cc}: expected 0 got ${code}'
		mut expected := []string{}
		for line in s.script.split_into_lines() {
			trimmed := line.trim_space()
			if trimmed.starts_with('.\\v.exe ') {
				expected << trimmed['.\\v.exe '.len..]
			}
		}
		assert calls == expected, '${s.cc}: calls mismatch'
	}
	println('test_success_keeps_all_follow_up_commands: OK')

	mut stats_scripts := []BuildScript{}
	for s in scripts {
		if s.script.contains('-stats') { stats_scripts << s }
	}
	assert stats_scripts.len == 1, 'expected 1 stats script got ${stats_scripts.len}'
	code, calls := run_build(root, stats_scripts[0].script, 0, 0, 49)
	assert code == 49, 'expected 49 got ${code}'
	assert calls.len == 2, 'expected 2 calls got ${calls.len}'
	println('test_final_ecdsa_failure_is_preserved: OK')

	println('All BatchExecutionTests: PASSED')
}
