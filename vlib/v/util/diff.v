module util

import v.util.diff

// iterates through a list of known diff cli commands
// and returns it with basic options
pub fn find_working_diff_command() !string {
	return diff.find_working_diff_command()
}

pub fn color_compare_files(diff_cmd string, file1 string, file2 string) string {
	return diff.color_compare_files(diff_cmd, file1, file2)
}

pub fn color_compare_strings(diff_cmd string, unique_prefix string, expected string, found string) string {
	return diff.color_compare_strings(diff_cmd, unique_prefix, expected, found)
}
