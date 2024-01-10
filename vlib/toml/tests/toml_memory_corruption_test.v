// This tests the `toml` module for a known memory corruption.
// The BUG shows below if no string `.clone()` nor any garbage-collection is done...
import os
import toml

const toml_text = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata', 'toml_test')) +
	'.toml') or { panic(err) }

fn test_toml_known_memory_corruption() {
	toml_doc := toml.parse_text(toml_text) or { panic(err) }

	owner := toml_doc.value('owner') as map[string]toml.Any
	any_name := owner.value('name')
	// This assert code path will cause the corruption.
	assert any_name.string() == 'Tom Preston-Werner'

	// This code then triggered the bug before the fix.
	// Also see note in toml/any.v in function `pub fn (a Any) string() string`
	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'

	// Repeat the pattern
	assert any_name.string() == 'Tom Preston-Werner'
	assert toml_doc.value('owner.name') as string == 'Tom Preston-Werner'
}

fn test_toml_known_memory_corruption_2() {
	toml_txt := '
	# Local Date-Time
	ldt1 = 1979-05-27T07:32:00
	ldt2 = 1979-05-27T00:32:00.999999
	# Local Date
	ld1 = 1979-05-27
	# Local Time
	lt1 = 07:32:00
	lt2 = 00:32:00.999999
'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }

	// ldt1 test section
	odt_time := toml.DateTime{'1979-05-27T07:32:00'}
	ldt1 := toml_doc.value('ldt1')
	assert ldt1.string() == '1979-05-27T07:32:00' // Running this before yielded a double free segfault
	assert ldt1.datetime() == odt_time
	assert ldt1.string() == '1979-05-27T07:32:00' // Used to yield a memory corruption

	// ld1 test section
	od_time := toml.Date{'1979-05-27'}
	ld1 := toml_doc.value('ld1')
	assert ld1.date() == od_time
	assert ld1.string() == '1979-05-27' // Used to yield a memory corruption

	// lt1 test section
	mut ot_time := toml.Time{'07:32:00'}
	lt1 := toml_doc.value('lt1')
	assert lt1.time() == ot_time
	assert lt1.string() == '07:32:00' // Used to yield a memory corruption

	// lt2 test section
	ot_time = toml.Time{'00:32:00.999999'}
	lt2 := toml_doc.value('lt2')
	assert lt2.time() == ot_time
	assert lt2.string() == '00:32:00.999999' // Used to yield a memory corruption
}
