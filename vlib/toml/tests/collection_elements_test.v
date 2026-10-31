import toml

enum JobTitle {
	worker
	executive
}

struct Item {
	name string
}

struct Collections {
	titles     []JobTitle
	by_title   map[string]JobTitle
	tables     []map[string]int
	texts      []map[string]string
	by_table   map[string]map[string]int
	by_list    map[string][]int
	lists      [][]int
	matrix     []map[string][][]int
	structs    []Item
	by_struct  map[string]Item
	timestamps []toml.DateTime
	by_day     map[string]toml.Date
}

// Note that TOML keys written after a `[table]` header belong to that table, so
// the root level keys come first.
const toml_text = 'titles = [0, 1, 0]
lists = [[1, 2], [3]]
timestamps = [2026-01-02T03:04:05Z]

[by_title]
main = 1

[by_table]
main = { x = 7 }

[by_list]
nums = [1, 2, 3]

[by_day]
start = 2026-01-02

[[tables]]
x = 1
[[tables]]
x = 2

[[texts]]
label = "a"

[[structs]]
name = "first"

[by_struct]
one = { name = "second" }
'

fn test_decode_enum_in_array() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.titles == [JobTitle.worker, JobTitle.executive, JobTitle.worker]
}

fn test_decode_enum_in_map() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.by_title == {
		'main': JobTitle.executive
	}
}

// A non-integer does not fit an enum element. `Any.int()` coerces booleans and
// floats and folds every other value to 0, so before this guard these elements
// were read as `worker` or `executive` instead of being skipped.
fn test_decode_enum_in_array_skips_mismatched() {
	c := toml.decode[Collections]('titles = [0, "skip", true, 1, 1.5, { x = 1 }, [1], 0]') or {
		panic(err)
	}
	assert c.titles == [JobTitle.worker, JobTitle.executive, JobTitle.worker]
}

fn test_decode_enum_in_map_skips_mismatched() {
	c := toml.decode[Collections]('[by_title]
zero = 0
one = 1
text = "skip"
flag = true
fraction = 1.5
table = { x = 1 }
list = [1]
') or {
		panic(err)
	}
	assert c.by_title == {
		'zero': JobTitle.worker
		'one':  JobTitle.executive
	}
}

fn test_decode_enum_map_with_all_mismatched_values_is_empty() {
	c := toml.decode[Collections]('[by_title]
text = "skip"
flag = true
') or {
		panic(err)
	}
	assert c.by_title.len == 0
}

fn test_decode_maps_in_array() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.tables == [
		{
			'x': 1
		}
		{
			'x': 2
		},
	]
	assert c.texts == [{
		'label': 'a'
	}]
}

fn test_decode_map_of_maps() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.by_table == {
		'main': {
			'x': 7
		}
	}
}

fn test_decode_map_of_arrays() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.by_list == {
		'nums': [1, 2, 3]
	}
}

fn test_decode_arrays_of_arrays() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.lists == [
		[1, 2]
		[3],
	]
}

fn test_decode_struct_collections_still_work() {
	c := toml.decode[Collections](toml_text) or { panic(err) }
	assert c.structs == [Item{'first'}]
	assert c.by_struct == {
		'one': Item{'second'}
	}
	assert c.timestamps == [toml.DateTime{'2026-01-02T03:04:05Z'}]
	assert c.by_day == {
		'start': toml.Date{'2026-01-02'}
	}
}

fn test_decode_skips_non_table_in_map_array() {
	c := toml.decode[Collections]('tables = [1, { x = 3 }, "y"]') or { panic(err) }
	// The elements that are not tables are skipped.
	assert c.tables == [{
		'x': 3
	}]
}

fn test_decode_map_skips_non_table_value() {
	c := toml.decode[Collections]('[by_table]\nnope = 1\nyes = { x = 2 }\n') or { panic(err) }
	assert c.by_table == {
		'yes': {
			'x': 2
		}
	}
}

// `Any.array()` wraps a scalar in a singleton array and turns a table into its
// values, so a source shape that is not an array must be skipped rather than
// coerced into one.
fn test_decode_skips_non_array_element() {
	c := toml.decode[Collections]('lists = [1, [2, 3], { x = 4 }, "skip", [], [5]]') or {
		panic(err)
	}
	assert c.lists == [
		[2, 3]
		[]
		[5],
	]
}

fn test_decode_map_skips_non_array_value() {
	c := toml.decode[Collections]('[by_list]\nscalar = 1\ntable = { x = 2 }\ntext = "skip"\nempty = []\nvalid = [3, 4]\n') or {
		panic(err)
	}
	assert c.by_list == {
		'empty': []int{}
		'valid': [3, 4]
	}
}

fn test_decode_skips_non_array_element_in_nested_map_array() {
	c := toml.decode[Collections]('matrix = [{ keep = [[1], 2, []], bad = 3 }]') or {
		panic(err)
	}
	assert c.matrix == [{
		'keep': [
			[1]
			[],
		]
	}]
}

fn test_decode_collections_round_trip() {
	original := Collections{
		titles:   [JobTitle.worker, JobTitle.executive]
		tables:   [{
			'x': 1
		}]
		by_table: {
			'main': {
				'x': 7
			}
		}
		by_list:  {
			'nums': [1, 2, 3]
		}
		lists:    [[1, 2], [3]]
		structs:  [Item{'first'}]
	}
	encoded := toml.encode[Collections](original)
	assert toml.decode[Collections](encoded)! == original
}
