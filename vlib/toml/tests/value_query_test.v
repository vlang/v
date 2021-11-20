import toml

const toml_text = '
modules = [ "ui", "toml" ]
errors = []

[[themes]]
name = "Dracula"
colors = [ "red", "black", "white" ]

[[themes]]
name = "Lemon"
colors = [
	"green",
	"yellow",
	[ "transparent" ]
]
'

fn test_value_query_in_array() {
	toml_doc := toml.parse(toml_text) or { panic(err) }
	mut value := toml_doc.value('themes[0].colors[1]').string()
	assert value == 'black'
	value = toml_doc.value('themes[1].colors[0]').string()
	assert value == 'green'
	value = toml_doc.value('themes[1].colors[2].[0]').string()
	assert value == 'transparent'
	value = toml_doc.value('modules[1]').string()
	assert value == 'toml'
	value = toml_doc.value('errors[11]').default_to('<none>').string()
	assert value == '<none>'
}
