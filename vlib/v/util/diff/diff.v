module diff

import os
// import term
import arrays.diff as arrays_diff

// compare_files returns a string displaying the differences between two files.
pub fn compare_files(path1 string, path2 string) !string {
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
pub fn compare_text(text1 string, text2 string) string {
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
