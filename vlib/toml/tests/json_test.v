import os
import toml

const toml_text = '
v = true

animal = { type.name = "pug" }

inline = { "a" = 4, "b.c" = 6, b.c = 7 }

[db]
t = true

[ij]
  [ij.a]
  i = 1
  j = 2

  [ij.b]
  i = "3"
  j = "4"

[fruit]
apple.color = "red"
apple.taste.sweet = true

[fruit.apple.texture]
smooth = true'

fn test_parse() {
	toml_doc := toml.parse(toml_text) or { panic(err) }

	toml_json := toml_doc.to_json()
	out_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.out'
	out_file_json := os.read_file(out_file) or { panic(err) }
	println(toml_json)
	assert toml_json == out_file_json

	// assert false
}
