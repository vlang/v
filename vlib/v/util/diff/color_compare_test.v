module diff

import os

// The deprecated helpers are thin wrappers now; what matters is that they still
// route to the replacement and never to a disabled external diff command.
fn test_available_tools_and_find_working_diff_command_are_retired() {
	assert available_tools() == []
	if _ := find_working_diff_command() {
		assert false, 'the deprecated lookup should always error'
	} else {
		assert err.msg() == 'deprecated'
	}
}

fn test_color_compare_strings_reports_the_difference() {
	patch := color_compare_strings('', '', 'a\nb\n', 'a\nc\n')
	assert patch.contains('b'), patch
	assert patch.contains('c'), patch
	assert patch.contains('-'), patch
	assert patch.contains('+'), patch
	// Identical texts produce no diff body.
	assert color_compare_strings('', '', 'same\n', 'same\n') == ''
}

fn test_color_compare_files_reports_the_difference() {
	root := os.join_path(os.vtmp_dir(), 'diff_color_files_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first.txt')
	second := os.join_path(root, 'second.txt')
	os.write_file(first, 'line one\nline two\n') or { panic(err) }
	os.write_file(second, 'line one\nline 2\n') or { panic(err) }
	patch := color_compare_files('', first, second)
	assert patch.contains('line two'), patch
	assert patch.contains('line 2'), patch
	// A file that cannot be read yields an empty patch, not an error.
	assert color_compare_files('', first, os.join_path(root, 'missing.txt')) == ''
}

fn test_compare_text_and_compare_files_agree() {
	root := os.join_path(os.vtmp_dir(), 'diff_compare_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	left := os.join_path(root, 'left.txt')
	right := os.join_path(root, 'right.txt')
	os.write_file(left, 'a\nb\n') or { panic(err) }
	os.write_file(right, 'a\nc\n') or { panic(err) }
	assert compare_files(left, right) or { panic(err) } == compare_text('a\nb\n', 'a\nc\n') or {
		panic(err)
	}
}
