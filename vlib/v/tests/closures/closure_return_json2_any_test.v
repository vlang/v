import json2
import net.urllib

fn test_closure_returns_array_of_imported_sum_type() {
	parts := fn (s string) []json2.Any {
		return s.split(',').map(json2.Any(it))
	}
	assert json2.encode(parts('a,b')) == '["a","b"]'
	assert json2.encode(parts('')) == '[""]'
	copy := parts
	assert json2.encode(copy('c,d')) == '["c","d"]'
}

fn test_closure_name_shadows_imported_function() {
	split := fn (s string) []json2.Any {
		return s.split(',').map(json2.Any(it))
	}
	assert json2.encode(split('one,two')) == '["one","two"]'
	assert urllib.path_escape('a b') == 'a%20b'
}

fn test_captured_closure_returns_imported_sum() {
	prefix := 'item:'
	parts := fn [prefix] (s string) []json2.Any {
		return s.split(',').map(json2.Any(prefix + it))
	}
	assert json2.encode(parts('a,b')) == '["item:a","item:b"]'
}
