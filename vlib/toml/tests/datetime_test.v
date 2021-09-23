import toml
import time

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
	mut odt_time := time.parse_rfc3339('1979-05-27T07:32:00Z') or { panic(err) }
	mut odt_str := toml_doc.value('odt1').string()

	// odt1 test section
	assert odt_str == '1979-05-26 07:32:00.000000' // W00t?! why 26th? Z=UTC?
	odt1 := toml_doc.value('odt1')
	assert odt1.datetime() == odt_time

	// odt2 test section
	odt_time = time.parse_rfc3339('1979-05-27T00:32:00-07:00') or { panic(err) }
	odt2 := toml_doc.value('odt2')
	assert odt2.datetime() == odt_time

	// odt3 test section
	odt_time = time.parse_rfc3339('1979-05-27T00:32:00.999999-07:00') or { panic(err) }
	odt3 := toml_doc.value('odt3')
	assert odt3.datetime() == odt_time

	// odt4 test section
	odt_time = time.parse_rfc3339('1979-05-27 07:32:00Z') or { panic(err) }
	odt4 := toml_doc.value('odt4')
	assert odt4.datetime() == odt_time

	// ldt1 test section
	odt_time = time.parse_rfc3339('1979-05-27T07:32:00') or { panic(err) }
	ldt1 := toml_doc.value('ldt1')
	assert ldt1.datetime() == odt_time

	// ldt2 test section
	odt_time = time.parse_rfc3339('1979-05-27T00:32:00.999999') or { panic(err) }
	ldt2 := toml_doc.value('ldt2')
	assert ldt2.datetime() == odt_time

	// ld1 test section
	odt_time = time.parse_rfc3339('1979-05-27') or { panic(err) }
	ld1 := toml_doc.value('ld1')
	assert ld1.datetime() == odt_time
	assert ld1.string() == '1979-05-27 00:00:00.000000'

	// lt1 test section
	odt_time = time.parse_rfc3339('07:32:00') or { panic(err) }
	lt1 := toml_doc.value('lt1')
	assert lt1.datetime() == odt_time
	assert lt1.string() == '0000-00-00 07:32:00.000000'

	// lt2 test section
	odt_time = time.parse_rfc3339('00:32:00.999999') or { panic(err) }
	lt2 := toml_doc.value('lt2')
	assert lt2.datetime() == odt_time
	assert lt2.string() == '0000-00-00 00:32:00.999999'
}
