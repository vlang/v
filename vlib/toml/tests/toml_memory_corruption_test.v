// This tests the `toml` module for a known memory corruption.
// The BUG shows below if no string `.clone()` nor any garbage-collection is done...
import os
import toml

const toml_text = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata', 'toml_test')) +
	'.toml') or { panic(err) }

fn test_toml_known_memory_corruption() {
	toml_doc := toml.parse(toml_text) or { panic(err) }

	owner := toml_doc.value('owner') as map[string]toml.Any
	any_name := owner.value('name') or { panic(err) }
	// This assert code path will cause the corruption.
	assert any_name.string() == 'Tom Preston-Werner'

	// This code then triggered the bug before the fix.
	// Also see note in toml/any.v in function `pub fn (a Any) string() string`
	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'

	// Repeat the pattern
	assert any_name.string() == 'Tom Preston-Werner'
	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'
}
