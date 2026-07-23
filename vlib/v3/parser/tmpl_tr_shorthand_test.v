module parser

// expand_veb_tr_shorthand must rewrite each `%key` translation marker independently,
// splicing only the range it just scanned. When one key is a prefix of another on the same
// line (`%title %title_long`), a global whole-line replace of the shorter key would also
// rewrite the prefix inside the longer marker, corrupting `%title_long` into the `title`
// interpolation followed by a stray `_long` before it can be scanned as its own key.
fn test_expand_veb_tr_shorthand_prefix_key_not_corrupted() {
	got := expand_veb_tr_shorthand('%title %title_long')
	expected := '@{veb.tr(ctx.lang.str(), "title")} @{veb.tr(ctx.lang.str(), "title_long")}'
	assert got == expected, got
}

// The same prefix hazard exists for the `%raw key` form.
fn test_expand_veb_tr_shorthand_raw_prefix_key_not_corrupted() {
	got := expand_veb_tr_shorthand('%raw title %raw title_long')
	expected := '@{veb.raw(veb.tr(ctx.lang.str(), "title"))} @{veb.raw(veb.tr(ctx.lang.str(), "title_long"))}'
	assert got == expected, got
}

// A single repeated key still expands every occurrence.
fn test_expand_veb_tr_shorthand_repeated_key() {
	got := expand_veb_tr_shorthand('%title %title')
	expected := '@{veb.tr(ctx.lang.str(), "title")} @{veb.tr(ctx.lang.str(), "title")}'
	assert got == expected, got
}

// A bare `%` not followed by a valid key is left untouched.
fn test_expand_veb_tr_shorthand_bare_percent_untouched() {
	assert expand_veb_tr_shorthand('100% done') == '100% done'
}
