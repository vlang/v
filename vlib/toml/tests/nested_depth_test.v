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
