// `make` runs this file after cache_basic_modules.v to populate the separate
// test configuration. Tests can then link the builtin, strings, strconv, hash
// and bits bundle without rebuilding it.
import strconv
import strings

fn test_cached_basic_modules() {
	assert strconv.atoi('42')! == 42
	assert strings.repeat(120, 2) == 'xx'
}
