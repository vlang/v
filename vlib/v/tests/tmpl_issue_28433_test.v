// A variable whose name merely starts with a directive's is an interpolation,
// not the directive: `@form_method` was compiled as a `@for` loop header, and
// `@end_note` and `@elsewhere` as `@end` and `@else`.
//
// The same template closes its blocks with `@endif` and `@endfor`, which
// TEMPLATES.md documents alongside `@end`. A plain word-boundary rule would
// reject both, so the test that the fix works and the test that it did not
// break the documented spellings are deliberately the same template.
fn render_28433() string {
	form_method := 'POST'
	end_note := 'note'
	elsewhere := 'other'
	show_form := true
	entries := ['a', 'b']
	return $tmpl('tmpl/directive_word_boundary_28433.html')
}

fn test_a_variable_may_start_with_a_directive_name() {
	out := render_28433()
	assert out.contains('METHOD="POST"'), out
	assert out.contains('<p>note</p>'), out
	assert out.contains('<span>other</span>'), out
}

fn test_endif_and_endfor_still_close_their_blocks() {
	out := render_28433()
	assert out.contains('<li>a</li>'), out
	assert out.contains('<li>b</li>'), out
	assert !out.contains('@end'), out
}
