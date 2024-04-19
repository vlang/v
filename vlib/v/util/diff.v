module util

import v.util.diff

// find_working_diff_command returns the first available command from a list of known diff cli tools.
@[deprecated_after: '2024-05-31']
@[deprecated]
pub fn find_working_diff_command() !string {
	return diff.find_working_diff_command()
}

// color_compare_files returns a colored diff between two files.
@[deprecated: 'use `diff.compare_files` instead']
@[deprecated_after: '2024-05-31']
pub fn color_compare_files(diff_cmd string, path1 string, path2 string) string {
	return diff.color_compare_files(diff_cmd, path1, path2)
}

// color_compare_strings returns a colored diff between two strings.
@[deprecated: 'use `diff.compare_text` instead']
@[deprecated_after: '2024-05-31']
pub fn color_compare_strings(diff_cmd string, unique_prefix string, expected string, found string) string {
	return diff.color_compare_strings(diff_cmd, unique_prefix, expected, found)
}
