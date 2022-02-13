import os
import toml
import toml.to
import toml.ast

const empty_toml_document = toml.Doc{
	ast: &ast.Root(0)
}

const (
	toml_text_with_utf8_bom  = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata',
		'toml_with_utf8_bom' + '.toml'))) or { panic(err) }
	toml_text_with_utf16_bom = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata',
		'toml_with_utf16_bom' + '.toml'))) or { panic(err) }
	toml_text_with_utf32_bom = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata',
		'toml_with_utf32_bom' + '.toml'))) or { panic(err) }
)

fn test_toml_with_bom() {
	toml_doc := toml.parse(toml_text_with_utf8_bom) or { panic(err) }
	toml_json := to.json(toml_doc)

	title := toml_doc.value('title')
	assert title == toml.Any('TOML Example')
	assert title as string == 'TOML Example'

	owner := toml_doc.value('owner') as map[string]toml.Any
	any_name := owner.value('name')
	assert any_name.string() == 'Tom Preston-Werner'

	database := toml_doc.value('database') as map[string]toml.Any
	db_serv := database['server'] or {
		panic('could not access "server" index in "database" variable')
	}
	assert db_serv as string == '192.168.1.1'

	// Re-cycle bad_toml_doc
	mut bad_toml_doc := empty_toml_document
	bad_toml_doc = toml.parse(toml_text_with_utf16_bom) or {
		println('     $err.msg()')
		assert true
		empty_toml_document
	}

	bad_toml_doc = toml.parse(toml_text_with_utf32_bom) or {
		println('     $err.msg()')
		assert true
		empty_toml_document
	}
}
