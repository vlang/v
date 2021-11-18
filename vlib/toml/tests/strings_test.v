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
	mut toml_doc := toml.parse(toml_multiline_text_1) or { panic(err) }

	mut value := toml_doc.value('multi1') or { panic(err) }
	assert value.string() == 'one'
	value = toml_doc.value('multi2') or { panic(err) }
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3') or { panic(err) }
	assert value.string() == 'one\ntwo\nthree'
	value = toml_doc.value('multi4') or { panic(err) }
	assert value.string() == 'one\ntwo\nthree\nfour\n'

	toml_doc = toml.parse(toml_multiline_text_2) or { panic(err) }
	value = toml_doc.value('multi1') or { panic(err) }
	assert value.string() == 'one'
	value = toml_doc.value('multi2') or { panic(err) }
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3') or { panic(err) }
	assert value.string() == 'one\ntwo\nthree'
	value = toml_doc.value('multi4') or { panic(err) }
	assert value.string() == 'one\ntwo\nthree\nfour\n'

	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc = toml.parse(toml_file) or { panic(err) }
	value = toml_doc.value('lit_one') or { panic(err) }
	assert value.string() == "'one quote'"
	value = toml_doc.value('lit_two') or { panic(err) }
	assert value.string() == "''two quotes''"
	value = toml_doc.value('mismatch1') or { panic(err) }
	assert value.string() == 'aaa' + "'''" + 'bbb'
	value = toml_doc.value('mismatch2') or { panic(err) }
	assert value.string() == 'aaa' + '"""' + 'bbb'
}

fn test_unicode_escapes() {
	mut toml_doc := toml.parse(toml_unicode_escapes) or { panic(err) }

	mut value := toml_doc.value('short') or { panic(err) }
	assert value.string() == r'\u03B4'
	value = toml_doc.value('long') or { panic(err) }
	assert value.string() == r'\U000003B4'
}

fn test_literal_strings() {
	toml_file :=
		os.real_path(os.join_path(os.dir(@FILE), 'testdata', os.file_name(@FILE).all_before_last('.'))) +
		'.toml'
	toml_doc := toml.parse(toml_file) or { panic(err) }

	assert toml_doc.value('lit1') or { panic(err) }.string() == r'\' // '\'
	assert toml_doc.value('lit2') or { panic(err) }.string() == r'\\' // '\\'
	assert toml_doc.value('lit3') or { panic(err) }.string() == r'\tricky\' // '\tricky\'

	// NOTE to Windows users: git is set to use Unix EOLs for all TOML files (*.toml) in the repo.
	// See `.gitattributes` in the project root for the rule in action.
	// These lines would look like this on Windows:
	// assert toml_doc.value('ml_lit1') or { panic(err) }.string() == '\r\n\\'
	assert toml_doc.value('ml_lit1') or { panic(err) }.string() == '\\'
	assert toml_doc.value('ml_lit2') or { panic(err) }.string() == '\\\n\\'
	assert toml_doc.value('ml_lit3') or { panic(err) }.string() == '\\\ntricky\\\n'
}
