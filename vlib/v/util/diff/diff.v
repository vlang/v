module diff

import os
import time

// iterates through a list of known diff cli commands
// and returns it with basic options
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
	for diffcmd in known_diff_tools {
		if diffcmd == 'opendiff' {
			os.find_abs_path_of_executable('opendiff') or { continue }
			return diffcmd
		}
		$if freebsd || openbsd {
			if diffcmd == 'diff' { // FreeBSD/OpenBSD diff have no `--version` option
				return diffcmd
			}
		}
		p := os.execute('${diffcmd} --version')
		if p.exit_code < 0 {
			continue
		}
		if p.exit_code == 0 { // success
			// TODO: proper implemenation of --color flag
			$if !macos {
				if diffcmd in ['gdiff', 'diff'] {
					if p.output.contains('GNU diffutils') && env_diffopts == '' {
						return '${diffcmd} --color=always'
					}
				}
			}
			if diffcmd in ['code', 'code.cmd'] {
				cmd := '${diffcmd} ${if env_diffopts != '' { env_diffopts } else { '-d' }}'
				return cmd
			}
			// Don't add spaces to the cmd if there are no `env_diffopts`.
			return if env_diffopts != '' { '${diffcmd} ${env_diffopts}' } else { diffcmd }
		}
	}
	return error('No working "diff" command found')
}

pub fn color_compare_files(diff_cmd string, path1 string, path2 string) string {
	os.find_abs_path_of_executable(diff_cmd.all_before(' ')) or {
		return 'comparison command: `${diff_cmd}` not found'
	}
	flags := $if openbsd {
		['-d', '-a', '-U', '2']
	} $else $if freebsd {
		['--minimal', '--text', '--unified=2']
	} $else {
		['--minimal', '--text', '--unified=2', '--show-function-line="fn "']
	}
	full_cmd := '${diff_cmd} ${flags.join(' ')} ${os.quoted_path(path1)} ${os.quoted_path(path2)}'
	return os.execute(full_cmd).output.trim_right('\r\n')
}

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
