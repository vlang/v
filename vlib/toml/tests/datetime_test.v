import toml

fn test_dates() {
	toml_txt := '
	# Offset Date-Time
	odt1 = 1979-05-27T07:32:00Z
	odt2 = 1979-05-27T00:32:00-07:00
	odt3 = 1979-05-27T00:32:00.999999-07:00
	odt4 = 1979-05-27 07:32:00Z
	# Local Date-Time
	ldt1 = 1979-05-27T07:32:00
	ldt2 = 1979-05-27T00:32:00.999999
	# Local Date
	ld1 = 1979-05-27
	# Local Time
	lt1 = 07:32:00
	lt2 = 00:32:00.999999
'
	toml_doc := toml.parse(toml_txt) or { panic(err) }

	// Re-use vars
	mut odt_time := toml.DateTime{'1979-05-27T07:32:00Z'}
	mut odt_str := toml_doc.value('odt1') or { panic(err) }.string()

	// odt1 test section
	assert odt_str == '1979-05-27T07:32:00Z'
	odt1 := toml_doc.value('odt1') or { panic(err) }
	assert odt1.datetime() == odt_time

	// odt2 test section
	odt_time = toml.DateTime{'1979-05-27T00:32:00-07:00'}
	odt2 := toml_doc.value('odt2') or { panic(err) }
	assert odt2.datetime() == odt_time

	// odt3 test section
	odt_time = toml.DateTime{'1979-05-27T00:32:00.999999-07:00'}
	odt3 := toml_doc.value('odt3') or { panic(err) }
	assert odt3.datetime() == odt_time

	// odt4 test section
	odt_time = toml.DateTime{'1979-05-27 07:32:00Z'}
	odt4 := toml_doc.value('odt4') or { panic(err) }
	assert odt4.datetime() == odt_time

	// ldt1 test section
	odt_time = toml.DateTime{'1979-05-27T07:32:00'}
	ldt1 := toml_doc.value('ldt1') or { panic(err) }
	assert ldt1.datetime() == odt_time

	// ldt2 test section
	odt_time = toml.DateTime{'1979-05-27T00:32:00.999999'}
	ldt2 := toml_doc.value('ldt2') or { panic(err) }
	assert ldt2.datetime() == odt_time

	// ld1 test section
	od_time := toml.Date{'1979-05-27'}
	ld1 := toml_doc.value('ld1') or { panic(err) }
	assert ld1.date() == od_time
	// assert ld1.string() == '1979-05-27' // TODO memory corruption

	// lt1 test section
	mut ot_time := toml.Time{'07:32:00'}
	lt1 := toml_doc.value('lt1') or { panic(err) }
	assert lt1.time() == ot_time
	// assert lt1.string() == '07:32:00' // TODO memory corruption

	// lt2 test section
	ot_time = toml.Time{'00:32:00.999999'}
	lt2 := toml_doc.value('lt2') or { panic(err) }
	assert lt2.time() == ot_time
	// assert lt2.string() == '00:32:00.999999'  // TODO memory corruption
}
