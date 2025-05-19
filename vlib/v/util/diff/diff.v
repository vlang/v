module diff

import os
// import term
import arrays.diff as arrays_diff

// compare_files returns a string displaying the differences between two files.
pub fn compare_files(path1 string, path2 string, _ CompareOptions) !string {
	src := os.read_lines(path1)!
	dst := os.read_lines(path2)!
	mut ctx := arrays_diff.diff(src, dst)
	patch := ctx.generate_patch(
		colorful:     true // term.can_show_color_on_stdout()
		block_header: true
		unified:      3
	)
	return patch
}

// compare_text returns a string displaying the differences between two strings.
pub fn compare_text(text1 string, text2 string, _ CompareTextOptions) !string {
	src := text1.split_into_lines()
	dst := text2.split_into_lines()
	mut ctx := arrays_diff.diff(src, dst)
	patch := ctx.generate_patch(
		colorful:     true // term.can_show_color_on_stdout()
		block_header: true
		unified:      3
	)
	return patch
}

// deprecated code :

pub enum DiffTool {
	auto
	diff      // core package on Unix-like systems.
	colordiff // `diff` wrapper.
	delta     // viewer for git and diff output.
	// fc // built-in tool on windows. // TODO: enable when its command output can be read.
}

@[params]
pub struct CompareOptions {
pub:
	tool DiffTool @[deprecated_after: '2025-12-31']
	// Custom args used with the diff command.
	args string @[deprecated_after: '2025-12-31']
	// Sets the environment variable whose value can overwrite a diff command passed to a compare function.
	// It also enables the use of commands that are not in the list of known diff tools.
	// Set it to `none` to disable it.
	env_overwrite_var ?string = 'VDIFF_CMD' @[deprecated_after: '2025-12-31']
}

@[params]
pub struct CompareTextOptions {
	CompareOptions
pub:
	base_name   string = 'base' @[deprecated_after: '2025-12-31']
	target_name string = 'target' @[deprecated_after: '2025-12-31']
}

// Allows public checking for the available tools and prevents repeated searches
// when using compare functions with automatic diff tool detection.
@[deprecated_after: '2025-12-31']
pub fn available_tools() []DiffTool {
	return []
}

@[deprecated_after: '2025-12-31']
pub fn find_working_diff_command() !string {
	return error('deprecated')
}

// color_compare_files returns a colored diff between two files.
@[deprecated: 'use compare_files instead']
@[deprecated_after: '2025-12-31']
pub fn color_compare_files(_ string, path1 string, path2 string) string {
	return compare_files(path1, path2) or { '' }
}

// color_compare_strings returns a colored diff between two strings.
@[deprecated: 'use compare_text instead']
@[deprecated_after: '2025-12-31']
pub fn color_compare_strings(_ string, _ string, expected string, found string) string {
	return compare_text(expected, found) or { '' }
}
