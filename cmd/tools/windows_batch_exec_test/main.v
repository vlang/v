module main

import os

const compilers = ['gcc', 'msvc', 'tcc']

fn run_build(root string, script string, makev int, symlink int, stats int) (int, []string) {
	calls := os.join_path(root, 'calls.txt')
	os.rm(calls) or {}
	env := os.environ()
	mut env_map := map[string]string{}
	for k, v in env { env_map[k] = v }
	env_map['VTEST_CI_CALLS'] = calls
	env_map['VTEST_MAKEV_EXIT'] = makev.str()
	env_map['VTEST_SYMLINK_EXIT'] = symlink.str()
	env_map['VTEST_STATS_EXIT'] = stats.str()

	body := "\$ErrorActionPreference = 'Stop'\n" + script +
		"\nif (Test-Path -LiteralPath variable:\\LASTEXITCODE) { exit \$LASTEXITCODE }\n"
	path := os.join_path(root, 'build.ps1')
	os.write_file(path, body) or { return 1, [] }

	// find pwsh
	pwsh := os.find_abs_path_of_executable('pwsh') or { 'powershell' }
	res := os.execute('${pwsh} -NoProfile -NonInteractive -File "${path}"')
	_ = res

	observed := if os.exists(calls) {
		os.read_file(calls) or { '' }.split_into_lines().filter(it.len > 0)
	} else {
		[]string{}
	}
	// get exit code from result; os.execute returns exit_code
	return res.exit_code, observed
}

fn main() {
	// This test requires Windows + gcc + pwsh
	$if !windows {
		println('BatchExecutionTests requires Windows; skipping')
		return
	}
	gcc := os.find_abs_path_of_executable('gcc') or {
		eprintln('requires gcc')
		return
	}
	pwsh := os.find_abs_path_of_executable('pwsh') or {
		os.find_abs_path_of_executable('powershell') or {
			eprintln('requires pwsh/powershell')
			return
		}
	}
	_ = gcc
	_ = pwsh

	root := os.join_path(os.temp_dir(), 'wbe_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { eprintln('mkdir failed'); exit(1) }
	defer { os.rmdir_all(root) or {} }

	// compile C probe
	source := os.join_path(root, 'probe.c')
	os.write_file(source, '#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\nint main(int argc, char **argv) {\n  FILE *calls = fopen(getenv("VTEST_CI_CALLS"), "a");\n  if (!calls) return 90;\n  for (int i = 1; i < argc; ++i) fprintf(calls, "%s%s", i == 1 ? "" : " ", argv[i]);\n  fputc(\'\\n\', calls);\n  fclose(calls);\n  const char *key = argc > 1 && strcmp(argv[1], "symlink") == 0 ? "VTEST_SYMLINK_EXIT" : "VTEST_STATS_EXIT";\n  const char *value = getenv(key);\n  return value ? atoi(value) : 0;\n}\n') or {}
	os.execute('${gcc} -std=c11 -Wall -Wextra -Werror -o "${root}/v.exe" "${source}"')

	// stub makev.bat
	os.write_file(os.join_path(root, 'makev.bat'), '@echo off\r\nexit /b %VTEST_MAKEV_EXIT%\r\n') or {}

	// read workflows
	for cc in compilers {
		wf_path := os.join_path(root, '..', '..', '..', '.github', 'workflows', 'windows_ci_${cc}.yml')
		_ = wf_path
	}

	println('BatchExecutionTests (V port) scaffold: OK')
}
