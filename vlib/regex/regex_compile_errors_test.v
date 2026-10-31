import regex

// One row of the error table: the pattern, the error code `regex_base` must
// report, and the offset in the pattern it must point at.
struct ErrCase {
	pattern string
	code    int
	pos     int
}

const err_cases = [
	// consecutive dots are rejected, use a quantifier instead
	ErrCase{r'..', regex.err_consecutive_dots, 1},
	ErrCase{r'a..b', regex.err_consecutive_dots, 2},
	// OR between two char classes
	ErrCase{r'[a]|[b]', regex.err_invalid_or_with_cc, 3},
	ErrCase{r'([a]|[b])*', regex.err_invalid_or_with_cc, 4},
	// unbalanced groups, in both directions
	ErrCase{r'(a', regex.err_group_not_balanced, 2},
	ErrCase{r'a)', regex.err_group_not_balanced, 2},
	ErrCase{r'((a)', regex.err_group_not_balanced, 2},
	// quantifier on a negation group
	ErrCase{r'(?!a)+', regex.err_neg_group_quantifier, 5},
	ErrCase{r'(?!a)*', regex.err_neg_group_quantifier, 5},
	// unsupported question mark group syntax
	ErrCase{r'(?x)a', regex.err_group_qm_notation, 2},
	ErrCase{r'(?)a', regex.err_group_qm_notation, 2},
	ErrCase{r'(?<n>a)', regex.err_group_qm_notation, 2},
	// unsupported backslash escapes
	ErrCase{r'\b', regex.err_syntax_error, 2},
	ErrCase{r'\q', regex.err_syntax_error, 2},
	ErrCase{r'\n', regex.err_syntax_error, 2},
	ErrCase{r'\', regex.err_syntax_error, 1},
	// unterminated char class
	ErrCase{r'[a', regex.err_syntax_error, 0},
	ErrCase{r'[]', regex.err_syntax_error, 0},
	// malformed {} bounds
	ErrCase{r'a{', regex.err_syntax_error, 1},
	ErrCase{r'a{1', regex.err_syntax_error, 1},
	ErrCase{r'a{,', regex.err_syntax_error, 1},
	ErrCase{r'a{}', regex.err_syntax_error, 1},
	// OR at the very end of the program
	ErrCase{r'a|', regex.err_syntax_error, 1},
]

fn test_invalid_patterns_report_the_expected_error_code() {
	for c in err_cases {
		_, re_err, err_pos := regex.regex_base(c.pattern)
		assert re_err == c.code, 'pattern "${c.pattern}": got ${re_err}, want ${c.code}'
		assert err_pos == c.pos, 'pattern "${c.pattern}": error at ${err_pos}, want ${c.pos}'
	}
}

// `compile_opt` wraps the same code in an `IError`, and formats a caret under
// the offending column of the query.
fn test_compile_opt_returns_a_coded_error_with_a_caret() {
	regex.regex_opt(r'ab[cd') or {
		assert err.code() == regex.err_syntax_error
		assert err.msg() == '\nquery: ab[cd\nerr  : --^\nERROR: err_syntax_error\n'
		return
	}
	assert false, 'expected the pattern to fail'
}

fn test_get_parse_error_string_names_every_code() {
	codes := [regex.compile_ok, regex.no_match_found, regex.err_char_unknown, regex.err_undefined,
		regex.err_internal_error, regex.err_cc_alloc_overflow, regex.err_syntax_error,
		regex.err_groups_overflow, regex.err_groups_max_nested, regex.err_group_not_balanced,
		regex.err_group_qm_notation, regex.err_invalid_or_with_cc, regex.err_neg_group_quantifier,
		regex.err_consecutive_dots]
	names := ['compile_ok', 'no_match_found', 'err_char_unknown', 'err_undefined', 'err_internal_error',
		'err_cc_alloc_overflow', 'err_syntax_error', 'err_groups_overflow', 'err_groups_max_nested',
		'err_group_not_balanced', 'err_group_qm_notation', 'err_invalid_or_with_cc',
		'err_neg_group_quantifier', 'err_consecutive_dots']
	mut re := regex.new()
	for i, code in codes {
		assert re.get_parse_error_string(code) == names[i], 'code ${code}'
	}
	assert re.get_parse_error_string(-99) == 'err_unknown'
}

fn test_error_constants_have_documented_values() {
	assert regex.compile_ok == 0
	assert regex.no_match_found == -1
	assert regex.err_char_unknown == -2
	assert regex.err_undefined == -3
	assert regex.err_internal_error == -4
	assert regex.err_cc_alloc_overflow == -5
	assert regex.err_syntax_error == -6
	assert regex.err_groups_overflow == -7
	assert regex.err_groups_max_nested == -8
	assert regex.err_group_not_balanced == -9
	assert regex.err_group_qm_notation == -10
	assert regex.err_invalid_or_with_cc == -11
	assert regex.err_neg_group_quantifier == -12
	assert regex.err_consecutive_dots == -13
}

// `regex_base` and `regex_opt` share `impl_compile`, so they must agree on
// which patterns compile, and `regex_opt`'s error code must be the one
// `regex_base` reported.
fn test_regex_opt_and_regex_base_agree_on_every_pattern() {
	good := [r'abc', r'\d+', r'^a$', r'[a-z]+', r'(a)(b)', r'(?:a)', r'(?P<n>a)', r'a{2,3}?', r'(?!a)\w+',
		r'a|b', r'\.']
	bad := [r'..', r'[a]|[b]', r'(a', r'a)', r'(?!a)+', r'(?x)a', r'\b', r'[a', r'a{', r'a|']
	for pattern in good {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err == regex.compile_ok, 'pattern "${pattern}" gave ${re_err}'
		regex.regex_opt(pattern) or { assert false, 'pattern "${pattern}" was rejected' }
	}
	for pattern in bad {
		_, re_err, _ := regex.regex_base(pattern)
		assert re_err != regex.compile_ok, 'pattern "${pattern}" unexpectedly compiled'
		regex.regex_opt(pattern) or {
			assert err.code() == re_err, 'pattern "${pattern}": regex_opt gave ${err.code()}, regex_base gave ${re_err}'
			continue
		}
		assert false, 'pattern "${pattern}" unexpectedly compiled'
	}
}

fn test_new_then_compile_opt_behaves_like_regex_opt() {
	mut re := regex.new()
	re.compile_opt(r'\d+') or { panic(err) }
	assert re.matches_string('123')
	assert !re.matches_string('abc')
	re.compile_opt(r'a{') or {
		assert err.code() == regex.err_syntax_error
		return
	}
	assert false, 'expected the second pattern to fail'
}

fn test_module_constants() {
	assert regex.v_regex_version == '1.0 alpha'
	assert regex.max_code_len == 256
	assert regex.max_quantifier == 1073741824
	assert regex.spaces == [` `, `\t`, `\n`, `\r`, `\v`, `\f`]
	assert regex.new_line_list == [`\n`, `\r`]
}
