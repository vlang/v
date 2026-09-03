module c

// These tests lock the two inlined-C-header scan optimizations:
//   * c_typedef_all_aggregate_aliases fuses the previous three separate
//     `typedef struct|union|enum` full-text passes into one.
//   * c_strip_comments copies non-comment content in runs instead of byte by byte.

fn sorted_unique(values []string) []string {
	mut seen := map[string]bool{}
	mut out := []string{}
	for v in values {
		if !seen[v] {
			seen[v] = true
			out << v
		}
	}
	out.sort()
	return out
}

// test_typedef_all_aggregate_aliases_matches_separate_scans is the core
// equivalence check: the fused scan must return exactly the same set of alias
// names as the union of the three original per-kind scans, for every typedef
// shape (tagged, bodyless `typedef struct tag Alias;`, braced body, comma
// separated declarators, and `structFoo`-style near-misses that must not match).
fn test_typedef_all_aggregate_aliases_matches_separate_scans() {
	samples := [
		'typedef struct S1 { int a; } A1;\n',
		'typedef union U1 { int a; float b; } A2, *A2p;\n',
		'typedef enum E1 { X, Y } A3;\n',
		'typedef struct tag Alias;\n',
		'typedef struct { int x; } Anon;\n',
		'struct plain { int a; };\ntypedef struct plain PlainAlias;\n',
		'typedef structure NotAnAggregate;\n', // `structure` must not match `struct`
		'typedef int Regular;\n', // plain typedef, no aggregate
		'/* c */ typedef struct C { int a; } WithComment;\n',
		'typedef struct S1 { int a; } A1;\ntypedef union U2 { int a; } A4;\ntypedef enum E2 { Z } A5;\n',
	]
	for sample in samples {
		fused := sorted_unique(c_typedef_all_aggregate_aliases(sample))
		mut separate := []string{}
		separate << c_typedef_struct_aliases(sample)
		separate << c_typedef_union_aliases(sample)
		separate << c_typedef_enum_aliases(sample)
		expected := sorted_unique(separate)
		assert fused == expected, 'mismatch for `${sample}`: fused=${fused} expected=${expected}'
	}
}

// test_typedef_all_aggregate_aliases_survives_unbalanced_brace_in_comment is a
// regression test: the scanner sees raw, comment-containing source, so a `{`
// inside a comment must not make c_matching_brace_end fail and terminate the
// whole fused pass, suppressing the following (valid) union alias.
fn test_typedef_all_aggregate_aliases_survives_unbalanced_brace_in_comment() {
	text := '/* example: typedef struct Broken { */\ntypedef union U { int x; } U;\n'
	got := c_typedef_all_aggregate_aliases(text)
	assert 'U' in got, 'union alias after a comment brace was dropped: ${got}'
	// Still exactly the union of the three independent scans (struct scan finds
	// nothing here, union scan finds U, enum scan finds nothing).
	mut separate := []string{}
	separate << c_typedef_struct_aliases(text)
	separate << c_typedef_union_aliases(text)
	separate << c_typedef_enum_aliases(text)
	assert sorted_unique(got) == sorted_unique(separate)
}

// test_typedef_all_aggregate_aliases_does_not_resume_inside_malformed_candidate
// is a regression test: after an unbalanced aggregate brace (here inside a
// comment), the scan must not resume within the malformed candidate and pick up
// a complete same-kind typedef that is also commented out. The independent
// struct scan stopped at the outer unmatched brace and never reached `Hidden`;
// collecting it would suppress the C.Hidden fallback declaration for a type the
// header never actually defines.
fn test_typedef_all_aggregate_aliases_does_not_resume_inside_malformed_candidate() {
	text := '/* typedef struct Broken { typedef struct Hidden { int x; } Hidden; */\n'
	got := c_typedef_all_aggregate_aliases(text)
	assert 'Hidden' !in got, 'commented-out struct after an unbalanced brace was collected: ${got}'
	// Exactly the union of the three independent scans (all empty here: the
	// struct scan breaks at the unmatched brace, there are no unions or enums).
	mut separate := []string{}
	separate << c_typedef_struct_aliases(text)
	separate << c_typedef_union_aliases(text)
	separate << c_typedef_enum_aliases(text)
	assert sorted_unique(got) == sorted_unique(separate)
}

// test_typedef_all_aggregate_aliases_ignores_brace_in_string_literal is a
// regression test: the fused scan sees raw C source, so an unbalanced `{` inside
// a string literal (a common shape in native C, e.g. `fprintf(f, "struct %s
// {\n", ...)`) must not inflate the tracked brace depth. Otherwise the depth
// stays > 0 and the following file-scope typedef looks block-scoped and is
// dropped from the owned typedef set.
fn test_typedef_all_aggregate_aliases_ignores_brace_in_string_literal() {
	text := 'int dump(FILE *f) {\n\tfprintf(f, "struct %s {");\n\treturn 0;\n}\ntypedef struct { int x; } StreamAlias;\n'
	got := c_typedef_all_aggregate_aliases(text)
	assert 'StreamAlias' in got, 'typedef after a string-literal brace was dropped: ${got}'
}

fn test_typedef_all_aggregate_aliases_collects_expected_names() {
	text := 'typedef struct S1 { int a; } A1;\ntypedef union U1 { int a; } A2;\ntypedef enum E1 { X } A3;\n'
	got := sorted_unique(c_typedef_all_aggregate_aliases(text))
	assert got == ['A1', 'A2', 'A3']
}

fn test_c_text_matches_at() {
	assert c_text_matches_at('typedef struct', 'typedef '.len, 'struct')
	assert !c_text_matches_at('typedef union', 'typedef '.len, 'struct')
	assert !c_text_matches_at('typedef str', 'typedef '.len, 'struct') // out of range
	assert c_text_matches_at('abc', 0, 'abc')
	assert !c_text_matches_at('abc', 1, 'abc')
}

fn test_c_strip_comments_removes_comments_and_keeps_content() {
	// Line comment: content before it survives, the newline is preserved.
	assert c_strip_comments('int x; // trailing\nint y;\n') == 'int x; \nint y;\n'
	// Block comment on one line is dropped, surrounding code kept.
	assert c_strip_comments('int /* mid */ z;\n') == 'int  z;\n'
	// No comments: identity.
	assert c_strip_comments('struct S { int a; };\n') == 'struct S { int a; };\n'
	// A lone slash is not a comment.
	assert c_strip_comments('int a = b / c;\n') == 'int a = b / c;\n'
}

// test_c_strip_comments_drops_unterminated_block_comment is a regression test:
// a block comment that runs to EOF must be discarded, not flushed back into the
// output (the final run flush must not run while still inside a comment).
fn test_c_strip_comments_drops_unterminated_block_comment() {
	assert c_strip_comments('int x; /* unfinished') == 'int x; '
	// Block-internal newlines are still preserved for line-count stability.
	assert c_strip_comments('a; /* open\nstill open') == 'a; \n'
}

fn test_c_strip_comments_preserves_block_comment_newlines() {
	// Multi-line block comments keep their internal newlines so declaration
	// scanning that relies on stable line counts is unaffected.
	input := 'a;\n/* line1\nline2\nline3 */\nb;\n'
	out := c_strip_comments(input)
	assert out.count('\n') == input.count('\n')
	assert out.contains('a;')
	assert out.contains('b;')
	assert !out.contains('line2')
}
