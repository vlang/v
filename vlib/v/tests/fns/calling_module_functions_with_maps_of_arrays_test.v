// vtest vflags: -w
import json2

fn test_calling_functions_with_map_initializations_containing_arrays() {
	result := json2.encode({
		// Note: []string{} should NOT be treated as []json.string{}
		'users':  []string{}
		'groups': []string{}
	}, escape_unicode: true)
	assert result == '{"users":[],"groups":[]}'
}
