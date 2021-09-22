import x.toml

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

	toml_multiline_text_3 = '# Make sure that quotes inside multiline strings are allowed, including right
# after the opening \'\'\'/""" and before the closing \'\'\'/"""

lit_one = \'\'\'\'one quote\'\'\'\'
lit_two = \'\'\'\'\'two quotes\'\'\'\'\'
lit_one_space = \'\'\' \'one quote\' \'\'\'
lit_two_space = \'\'\' \'\'two quotes\'\' \'\'\'

one = """"one quote""""
two = """""two quotes"""""
one_space = """ "one quote" """
two_space = """ ""two quotes"" """

mismatch1 = """aaa\'\'\'bbb"""
mismatch2 = \'\'\'aaa"""bbb\'\'\'
'
)

fn test_multiline_strings() {
	mut toml_doc := toml.parse(toml_multiline_text_1) or { panic(err) }

	mut value := toml_doc.value('multi1')
	assert value.string() == 'one'
	value = toml_doc.value('multi2')
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3')
	assert value.string() == '\none\ntwo\nthree'
	value = toml_doc.value('multi4')
	assert value.string() == '\none\ntwo\nthree\nfour\n'

	toml_doc = toml.parse(toml_multiline_text_2) or { panic(err) }
	value = toml_doc.value('multi1')
	assert value.string() == 'one'
	value = toml_doc.value('multi2')
	assert value.string() == 'one\ntwo'
	value = toml_doc.value('multi3')
	assert value.string() == '\none\ntwo\nthree'
	value = toml_doc.value('multi4')
	assert value.string() == '\none\ntwo\nthree\nfour\n'

	toml_doc = toml.parse(toml_multiline_text_3) or { panic(err) }
	value = toml_doc.value('lit_one')
	assert value.string() == "'one quote'"
	value = toml_doc.value('lit_two')
	assert value.string() == "''two quotes''"
	value = toml_doc.value('mismatch1')
	assert value.string() == 'aaa' + "'''" + 'bbb'
	value = toml_doc.value('mismatch2')
	assert value.string() == 'aaa' + '"""' + 'bbb'
}
