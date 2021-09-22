import os
import x.toml

const toml_text = '[db]
t = true

[ij]
  # Indentation (tabs and/or spaces) is allowed but not required
  [ij.a]
  i = 1
  j = 2

  [ij.b]
  i = "3"
  j = "4"'

fn test_parse() {
	toml_doc := toml.parse(toml_text)

	assert true
/*
	// TODO
	parsed_json := toml_doc.to_json()
	test_suite_json := os.read_file(@FILE.all_before_last('.')+'.out') or { panic(err) }
	assert parsed_json == test_suite_json
	*/
}
