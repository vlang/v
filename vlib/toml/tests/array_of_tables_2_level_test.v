import os
import toml
import toml.to

const toml_text = '[[albums]]
name = "Born to Run"

  [[albums.songs]]
  name = "Jungleland"

  [[albums.songs]]
  name = "Meeting Across the River"

[[albums]]
name = "Born in the USA"

  [[albums.songs]]
  name = "Glory Days"

  [[albums.songs]]
  name = "Dancing in the Dark"'

const fprefix = os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))

fn test_nested_array_of_tables() {
	mut toml_doc := toml.parse_text(toml_text)!

	toml_json := to.json(toml_doc)
	eprintln(toml_json)

	assert toml_json == os.read_file(fprefix + '.out')!
}

fn test_nested_array_of_tables_with_comment_before_key() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a.b]]
p = "1"  # comment
q = "2"
[[a.b]]
p = "3"
# comment
q = "4"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "b": [ { "p": "1", "q": "2" }, { "p": "3", "q": "4" } ] } ] }'
}

fn test_nested_array_of_tables_with_comment_before_parent_subtable() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a.b]]
p = "1"
# comment
[a.c]
q = "2"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "b": [ { "p": "1" } ], "c": { "q": "2" } } ] }'
}

fn test_nested_array_of_tables_with_comment_before_child_subtable() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a.b]]
p = "1"
# comment
[a.b.c]
q = "2"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "b": [ { "p": "1", "c": { "q": "2" } } ] } ] }'
}

fn test_nested_array_of_tables_with_numeric_dashed_key_child_subtable() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a.1-2]]
p = "1"
# comment
[a.1-2.c]
q = "2"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "1-2": [ { "p": "1", "c": { "q": "2" } } ] } ] }'
}

fn test_nested_array_of_tables_with_numeric_boolean_key_child_subtable() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a.1-true]]
p = "1"
# comment
[a.1-true.c]
q = "2"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "1-true": [ { "p": "1", "c": { "q": "2" } } ] } ] }'
}

fn test_nested_array_of_tables_with_escaped_quoted_key_child_subtable() {
	toml_doc := toml.parse_text('[[a]]
n = "x"
[[a."b\\"c"]]
p = "1"
# comment
[a."b\\"c".d]
q = "2"
')!

	toml_json := to.json(toml_doc)
	assert toml_json == '{ "a": [ { "n": "x", "b"c": [ { "p": "1", "d": { "q": "2" } } ] } ] }'
}
