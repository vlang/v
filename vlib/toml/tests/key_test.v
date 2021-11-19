import toml
import toml.to

const (
	toml_text = '34-11 = 23 # came out as "34-11 "
1.2 = 3 # came out as "1.2"
# TODO 34-12.2 = 42'
)

fn test_keys() {
	mut toml_doc := toml.parse(toml_text) or { panic(err) }

	mut value := toml_doc.value('34-11')
	assert value.int() == 23

	value = toml_doc.value('1.2')
	assert value.int() == 3

	// TODO
	// println(to.json(toml_doc))
	// value = toml_doc.value('34-12.2')
	// assert value.int() == 42
}
