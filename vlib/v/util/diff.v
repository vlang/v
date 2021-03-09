module util

import os
import time

// iterates through a list of known diff cli commands
// and returns it with basic options
pub fn find_working_diff_command() ?string {
	env_difftool := os.getenv('VDIFF_TOOL')
	env_diffopts := os.getenv('VDIFF_OPTIONS')
	mut known_diff_tools := []string{}
	if env_difftool.len > 0 {
		known_diff_tools << env_difftool
	}
	known_diff_tools << ['colordiff', 'gdiff', 'diff', 'colordiff.exe', 'diff.exe', 'opendiff',
		'code', 'code.cmd']
	// NOTE: code.cmd is the Windows variant of the `code` cli tool
	for diffcmd in known_diff_tools {
		if diffcmd == 'opendiff' { // opendiff has no `--version` option
			if opendiff_exists() {
				return diffcmd
			}
			continue
		}
		p := os.execute('$diffcmd --version')
		if p.exit_code < 0 {
			continue
		}
		if p.exit_code == 127 && diffcmd == env_difftool {
			// user setup is wonky, fix it
			return error('could not find specified VDIFF_TOOL $diffcmd')
		}
		if p.exit_code == 0 { // success
			if diffcmd in ['code', 'code.cmd'] {
				// there is no guarantee that the env opts exist
				// or include `-d`, so (harmlessly) add it
				return '$diffcmd $env_diffopts -d'
			}
			return '$diffcmd $env_diffopts'
		}
	}
	return error('No working "diff" command found')
}

// determine if the FileMerge opendiff tool is available
fn opendiff_exists() bool {
	o := os.execute('opendiff')
	if o.exit_code < 0 {
		return false
	}
	if o.exit_code == 1 { // failed (expected), but found (i.e. not 127)
		if o.output.contains('too few arguments') { // got some expected output
			return true
		}
	}
	return false
}

pub fn color_compare_files(diff_cmd string, file1 string, file2 string) string {
	if diff_cmd != '' {
		full_cmd := '$diff_cmd --minimal --text --unified=2  --show-function-line="fn " "$file1" "$file2" '
		x := os.execute(full_cmd)
		if x.exit_code < 0 {
			return 'comparison command: `$full_cmd` not found'
		}
		return x.output.trim_right('\r\n')
	}
	return ''
}

pub fn color_compare_strings(diff_cmd string, expected string, found string) string {
	cdir := os.cache_dir()
	ctime := time.sys_mono_now()
	e_file := os.join_path(cdir, '${ctime}.expected.txt')
	f_file := os.join_path(cdir, '${ctime}.found.txt')
	os.write_file(e_file, expected) or { panic(err) }
	os.write_file(f_file, found) or { panic(err) }
	res := color_compare_files(diff_cmd, e_file, f_file)
	os.rm(e_file) or { panic(err) }
	os.rm(f_file) or { panic(err) }
	return res
}
