module diff

import os
import time

// find_working_diff_command returns the first available command from a list of known diff cli tools.
pub fn find_working_diff_command() !string {
	env_difftool := os.getenv('VDIFF_TOOL')
	env_diffopts := os.getenv('VDIFF_OPTIONS')
	if env_difftool != '' {
		os.find_abs_path_of_executable(env_difftool) or {
			return error('could not find specified VDIFF_TOOL `${env_difftool}`')
		}
		return '${env_difftool} ${env_diffopts}'
	}
	known_diff_tools := ['colordiff', 'gdiff', 'diff', 'colordiff.exe', 'diff.exe', 'opendiff',
		'code', 'code.cmd'] // NOTE: code.cmd is the Windows variant of the `code` cli tool
	mut diff_cmd := ''
	for cmd in known_diff_tools {
		os.find_abs_path_of_executable('opendiff') or { continue }
		diff_cmd = cmd
		break
	}
	if diff_cmd == '' {
		return error('No working "diff" command found')
	}
	if diff_cmd in ['code', 'code.cmd'] {
		// Make sure the diff flag `-d` is included in any case.
		return '${diff_cmd} ${env_diffopts} -d'
	}
	// Don't add spaces to the cmd if there are no `env_diffopts`.
	return if env_diffopts != '' { '${diff_cmd} ${env_diffopts}' } else { diff_cmd }
}

// color_compare_files returns a colored diff between two files.
pub fn color_compare_files(diff_cmd string, path1 string, path2 string) string {
	cmd := diff_cmd.all_before(' ')
	os.find_abs_path_of_executable(cmd) or { return 'comparison command: `${cmd}` not found' }
	flags := $if openbsd {
		['-d', '-a', '-U', '2']
	} $else $if freebsd {
		['--minimal', '--text', '--unified=2']
	} $else {
		['--minimal', '--text', '--unified=2', '--show-function-line="fn "']
	}
	if cmd == 'diff' {
		color_diff_cmd := '${diff_cmd} --color=always ${flags.join(' ')} ${os.quoted_path(path1)} ${os.quoted_path(path2)}'
		color_result := os.execute(color_diff_cmd)
		if !color_result.output.starts_with('diff: unrecognized option') {
			return color_result.output.trim_right('\r\n')
		}
	}
	full_cmd := '${diff_cmd} ${flags.join(' ')} ${os.quoted_path(path1)} ${os.quoted_path(path2)}'
	return os.execute(full_cmd).output.trim_right('\r\n')
}

// color_compare_strings returns a colored diff between two strings.
pub fn color_compare_strings(diff_cmd string, unique_prefix string, expected string, found string) string {
	tmp_dir := os.join_path_single(os.vtmp_dir(), unique_prefix)
	os.mkdir(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	ctime := time.sys_mono_now()
	e_file := os.join_path_single(tmp_dir, '${ctime}.expected.txt')
	f_file := os.join_path_single(tmp_dir, '${ctime}.found.txt')
	os.write_file(e_file, expected) or { panic(err) }
	os.write_file(f_file, found) or { panic(err) }
	res := color_compare_files(diff_cmd, e_file, f_file)
	return res
}
