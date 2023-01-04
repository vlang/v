import os
import toml

const (
	toml_multiline_text_1 = 'multi1 = """one"""
multi2 = """one
two"""
multi3 = """
one
two
three"""
multi4 = """
one
two
three
four
"""'
	toml_multiline_text_2 = "multi1 = '''one'''
multi2 = '''one
two'''
multi3 = '''
one
two
three'''
multi4 = '''
one
two
three
four
'''"
	toml_unicode_escapes = r'short = "\u03B4"
long = "\U000003B4"'
)

fn test_multiline_strings() {
	mut toml_doc := toml.parse_text(toml_multiline_text_1) or { panic(err) }

	mut value := toml_doc.value('multi1')
	assert value.string() == 'one'
	value = toml_doc.value('multi2')
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3')
	assert value.string() == 'one\ntwo\nthree'
	value = toml_doc.value('multi4')
	assert value.string() == 'one\ntwo\nthree\nfour\n'

	toml_doc = toml.parse_text(toml_multiline_text_2) or { panic(err) }
	value = toml_doc.value('multi1')
	assert value.string() == 'one'
	value = toml_doc.value('multi2')
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3')
	assert value.string() == 'one\ntwo\nthree'
	value = toml_doc.value('multi4')
	assert value.string() == 'one\ntwo\nthree\nfour\n'

	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc = toml.parse_file(toml_file) or { panic(err) }
	value = toml_doc.value('lit_one')
	assert value.string() == "'one quote'"
	value = toml_doc.value('lit_two')
	assert value.string() == "''two quotes''"
	value = toml_doc.value('mismatch1')
	assert value.string() == 'aaa' + "'''" + 'bbb'
	value = toml_doc.value('mismatch2')
	assert value.string() == 'aaa' + '"""' + 'bbb'
}

fn test_unicode_escapes() {
	mut toml_doc := toml.parse_text(toml_unicode_escapes) or { panic(err) }

	mut value := toml_doc.value('short')
	assert value.string() == '\u03B4' // <- This escape is handled by V
	value = toml_doc.value('long')
	assert value.string() == 'δ' // <- for the long escape we compare with the unicode point
}

fn test_literal_strings() {
	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc := toml.parse_file(toml_file) or { panic(err) }

	assert toml_doc.value('lit1').string() == r'\' // '\'
	assert toml_doc.value('lit2').string() == r'\\' // '\\'
	assert toml_doc.value('lit3').string() == r'\tricky\' // '\tricky\'

	// NOTE to Windows users: git is set to use Unix EOLs for all TOML files (*.toml) in the repo.
	// See `.gitattributes` in the project root for the rule in action.
	// These lines would look like this on Windows:
	// assert toml_doc.value('ml_lit1').string() == '\r\n\\'
	assert toml_doc.value('ml_lit1').string() == '\\'
	assert toml_doc.value('ml_lit2').string() == '\\\n\\'
	assert toml_doc.value('ml_lit3').string() == '\\\ntricky\\\n'
}
