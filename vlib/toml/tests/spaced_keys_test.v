import toml

fn test_spaced_keys() {
	str_value := 'V rocks!'

	toml_txt := '
	  "o"   .   	pq  .  r     =    "Yuk"

[[ a . "b.c" ]]
	d . e = "V rocks!"

[ tube . test . "test.test" ]
	 h  .	"i.j."  .   "k"  =   	 "Cryptic"
'
	toml_doc := toml.parse_text(toml_txt) or { panic(err) }
	mut value := toml_doc.value('a."b.c"[0].d.e')
	assert value == toml.Any(str_value)
	assert value as string == str_value
	assert value.string() == str_value

	value = toml_doc.value('"o".pq.r')
	assert value.string() == 'Yuk'

	value = toml_doc.value('tube.test."test.test".h."i.j."."k"')
	assert value.string() == 'Cryptic'
}
