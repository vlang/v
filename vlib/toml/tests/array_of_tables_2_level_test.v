import os
import toml
import toml.to

const (
	toml_text = '[[albums]]
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
)

const fprefix = os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))

fn test_nested_array_of_tables() ? {
	mut toml_doc := toml.parse_text(toml_text)?

	toml_json := to.json(toml_doc)
	eprintln(toml_json)

	assert toml_json == os.read_file(fprefix + '.out')?
}
