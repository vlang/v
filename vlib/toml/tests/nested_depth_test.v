// The parser descends recursively into inline tables and arrays, so the
// stack bounds how deeply they can nest. The iarna suite has
// `values/qa-table-inline-nested-1000.toml` and
// `values/qa-array-inline-nested-1000.toml` (1001 levels each), which the
// toml CI job parses on the 8 MB main thread of Linux. Each document is
// parsed in a spawned thread here, because threads get an 8 MB stack on
// every 64-bit target, while the main thread of Windows has only 1 MB.
import toml

const levels = 1001

fn nested_tables(n int) string {
	return 'key = ' + '{key = '.repeat(n - 1) + '{}' + '}'.repeat(n - 1) + '\n'
}

fn nested_arrays(n int) string {
	return 'key = ' + '['.repeat(n) + ']'.repeat(n) + '\n'
}

fn nested_tables_with_dotted_key(n int) string {
	return 'key = ' + '{key = '.repeat(n - 1) + '{a.b = 1}' + '}'.repeat(n - 1) + '\n'
}

fn nested_arrays_of_tables(n int) string {
	return 'key = ' + '[{key = '.repeat(n) + '1' + '}]'.repeat(n) + '\n'
}

// parse_and_query parses `source` and returns the TOML text of the value at
// `key`, or `ok` when `key` is empty. Querying is recursive too, so it runs in
// the same thread as the parser.
fn parse_and_query(source string, key string) string {
	doc := toml.parse_text(source) or { return 'parse error: ${err}' }
	if key == '' {
		return 'ok'
	}
	return doc.value(key).to_toml()
}

fn parse_in_thread(source string, key string) string {
	t := spawn parse_and_query(source, key)
	return t.wait()
}

fn test_inline_tables_nested_1001_levels() {
	key := 'key.'.repeat(levels - 1) + 'key'
	assert parse_in_thread(nested_tables(levels), key) == '{ }'
}

fn test_arrays_nested_1001_levels() {
	assert parse_in_thread(nested_arrays(levels), '') == 'ok'
}

fn test_dotted_key_in_inline_tables_nested_1001_levels() {
	key := 'key.'.repeat(levels) + 'a.b'
	assert parse_in_thread(nested_tables_with_dotted_key(levels), key) == '1'
}

fn test_arrays_of_inline_tables_nested_500_levels() {
	key := 'key[0].'.repeat(500) + 'key'
	assert parse_in_thread(nested_arrays_of_tables(500), key) == '1'
}

// The nesting depth limit (max_nesting_depth in vlib/toml/parser/parser.v)
// turns unbounded parser recursion into a bounded parse error, instead of a
// stack-overflow process abort at deeper input (issue #31946). Keep this
// constant in sync with the parser.
const max_nesting_depth = 1024

fn at_limit_nested_arrays() string {
	return 'key = ' + '['.repeat(max_nesting_depth) + ']'.repeat(max_nesting_depth)
}

fn at_limit_nested_inline_tables() string {
	return 'key = ' + '{key = '.repeat(max_nesting_depth) + '0' + '}'.repeat(max_nesting_depth)
}

fn test_nesting_at_max_nesting_depth_still_parses() {
	// deep parse checks run in spawned threads: threads have an 8 MB stack
	// on every 64-bit target, while the main thread of Windows has 1 MB.
	assert parse_in_thread(at_limit_nested_arrays(), '') == 'ok'
	assert parse_in_thread(at_limit_nested_inline_tables(), '') == 'ok'
}

fn test_nesting_over_max_nesting_depth_errors_instead_of_crashing() {
	arr_bad := 'key = ' + '['.repeat(max_nesting_depth + 1) + ']'.repeat(max_nesting_depth + 1)
	if doc := toml.parse_text(arr_bad) {
		// a successful parse here would mean the nesting guard regressed
		// into a potential stack-overflow abort again
		_ = doc
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}

	tbl_bad := 'key = ' + '{key = '.repeat(max_nesting_depth + 1) + '0' + '}'.repeat(max_nesting_depth + 1)
	if doc := toml.parse_text(tbl_bad) {
		_ = doc
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
}

// mixed_nesting alternates array and inline-table containers along one
// chain, so the shared counter counts both forms in a single document.
fn mixed_nesting(n int) string {
	mut opens := []string{}
	for i in 0 .. n {
		opens << if i % 2 == 0 { '[' } else { '{key = ' }
	}
	mut closing := []string{}
	for i := n - 1; i >= 0; i-- {
		closing << if opens[i].len == 1 { ']' } else { '}' }
	}
	return 'key = ' + opens.join('') + '0' + closing.join('')
}

fn test_mixed_array_and_inline_table_nesting_at_both_boundaries() {
	assert parse_in_thread(mixed_nesting(max_nesting_depth), '') == 'ok'

	assert parse_in_thread(mixed_nesting(max_nesting_depth + 1), '').contains('maximum nesting depth')
}

// sibling containers each start from zero depth: without the deferred
// decrements, the second sibling would inherit the first one's depth.
fn test_sibling_containers_do_not_accumulate_depth() {
	sub := '['.repeat(max_nesting_depth - 1) + '0' + ']'.repeat(max_nesting_depth - 1)
	doc := 'key = [' + sub + ',' + sub + ',' + sub + ']'
	assert parse_in_thread(doc, '') == 'ok'
}
