module util

import os
import time

pub const (
	cmd_opendiff = 'opendiff'
)

// iterate through a list of known diff cli commands
pub fn find_working_diff_command() ?string {
	mut cmd_codediff := 'code'
	$if windows {
		cmd_codediff = 'code.cmd'
	}
	for diffcmd in ['colordiff', 'gdiff', 'diff', 'colordiff.exe', 'diff.exe', cmd_opendiff, cmd_codediff] {
		if diffcmd == cmd_opendiff {
			// opendiff has no `--version` option
			if opendiff_exists() {
				return diffcmd
			} else { // process next item
				continue
			}
		}
		diff_version_check := '$diffcmd --version'
		p := os.exec(diff_version_check) or {
			continue
		}
		if p.exit_code == 0 { // success
			if diffcmd == cmd_codediff {
				return '$diffcmd -d'
			}
			return diffcmd
		}
	}
	return error('no working diff command found')
}

// determine if the FileMerge opendiff tool is available
fn opendiff_exists() bool {
	o := os.exec(cmd_opendiff) or {
		return false
	}
	// not needed, kept for vebosity
	// if o.exit_code == 127 { // command not found
	// return false
	// }
	if o.exit_code == 1 { // failed, but found
		if o.output.contains('too few arguments') { // got a match
			return true
		}
	}
	return false
}

pub fn color_compare_files(diff_cmd, file1, file2 string) string {
	if diff_cmd != '' {
		mut other_options := os.getenv('VDIFF_OPTIONS')
		full_cmd := '$diff_cmd --minimal --text --unified=2 ' +
			' --show-function-line="fn " $other_options "$file1" "$file2" '
		x := os.exec(full_cmd) or {
			return 'comparison command: `$full_cmd` failed'
		}
		return x.output.trim_right('\r\n')
	}
	return ''
}

pub fn color_compare_strings(diff_cmd, expected, found string) string {
	cdir := os.cache_dir()
	ctime := time.sys_mono_now()
	e_file := os.join_path(cdir, '${ctime}.expected.txt')
	f_file := os.join_path(cdir, '${ctime}.found.txt')
	os.write_file(e_file, expected)
	os.write_file(f_file, found)
	res := color_compare_files(diff_cmd, e_file, f_file)
	os.rm(e_file)
	os.rm(f_file)
	return res
}
