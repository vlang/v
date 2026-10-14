module main

// filter_test covers the line-filtering rules that `v vet` uses to suppress
// false positive trailing-whitespace and space-indentation diagnostics inside
// comments and multi line assignments.
// `Vet::vet_file`/`vet_line` and the whole diagnostics pipeline are not unit
// testable here without spawning a nested `v` process.

// new_filtered_lines mirrors the map that `vet_file` builds: both ErrorType
// keys start empty and are filled in as comments and assignments are scanned.
fn new_filtered_lines() FilteredLines {
	mut fl := FilteredLines{}
	fl[.space_indent] = map[int]bool{}
	fl[.trailing_space] = map[int]bool{}
	return fl
}

fn test_comments_single_line_comment_filters_nothing() {
	mut fl := new_filtered_lines()
	fl.comments(false, 1, 3)
	assert fl[.space_indent].len == 0
	assert fl[.trailing_space].len == 0
}

fn test_comments_multiline_comment_hides_the_indented_body() {
	mut fl := new_filtered_lines()
	// A `/* ... */` comment spanning lines 1..3 filters only the inner lines.
	fl.comments(true, 1, 3)
	assert fl[.space_indent].len == 2
	assert fl[.space_indent][2]
	assert fl[.space_indent][3]
	assert !fl[.space_indent].keys().any(it in [1, 4])
	assert fl[.trailing_space].len == 0
}

fn test_comments_adjacent_span_filters_only_the_last_line() {
	mut fl := new_filtered_lines()
	fl.comments(true, 5, 6)
	assert fl[.space_indent].len == 1
	assert fl[.space_indent][6]
	assert fl[.trailing_space].len == 0
}

fn test_assigns_single_line_assignment_filters_nothing() {
	mut fl := new_filtered_lines()
	fl.assigns(5, 5)
	assert fl[.space_indent].len == 0
	assert fl[.trailing_space].len == 0
}

fn test_assigns_multiline_marks_the_ends_and_the_continuations() {
	mut fl := new_filtered_lines()
	// start=5, end=9: the continuation lines 6..8 get both kinds, while the
	// first line keeps trailing_space and the last line space_indent.
	fl.assigns(5, 9)
	assert fl[.trailing_space].len == 4
	assert fl[.trailing_space][5]
	assert fl[.trailing_space][6]
	assert fl[.trailing_space][7]
	assert fl[.trailing_space][8]
	assert fl[.space_indent].len == 4
	assert fl[.space_indent][6]
	assert fl[.space_indent][7]
	assert fl[.space_indent][8]
	assert fl[.space_indent][9]
}

fn test_assigns_two_line_assignment_keeps_both_endpoints() {
	mut fl := new_filtered_lines()
	fl.assigns(5, 6)
	assert fl[.trailing_space].len == 1
	assert fl[.trailing_space][5]
	assert fl[.space_indent].len == 1
	assert fl[.space_indent][6]
}
