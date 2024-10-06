import toml

const toml_text = '# This TOML can reflect/decode to a struct
val_string = "test"
val_bool = false
val_int = 456
val_i64 = 4567
val_u64 = 45678
val_f32 = 200.2
val_f64 = 2000.2
val_datetime = 2024-10-05 09:10:20.000
val_date = 2099-09-09
val_time = 22:22:22.222
'

const toml_all_default_text = '# This TOML can reflect/decode to a struct all with default values'

struct Test {
	val_string   string        = 'abcd'
	val_bool     bool          = true
	val_int      int           = 123
	val_i64      i64           = 1234
	val_u64      u64           = 12345
	val_f32      f32           = 100.1
	val_f64      f64           = 1000.1
	val_datetime toml.DateTime = toml.DateTime{'1980-07-11 21:23:42.123'}
	val_date     toml.Date     = toml.Date{'1977-07-07'}
	val_time     toml.Time     = toml.Time{'11:11:11.111'}
}

fn test_reflect_default_values() {
	toml_has_values := toml.parse_text(toml_text) or { panic(err) }
	test_has_values := toml_has_values.reflect[Test]()

	assert test_has_values.val_string == 'test'
	assert test_has_values.val_bool == false
	assert test_has_values.val_int == 456
	assert test_has_values.val_i64 == 4567
	assert test_has_values.val_u64 == 45678
	assert test_has_values.val_f32 == 200.2
	assert test_has_values.val_f64 == 2000.2
	assert test_has_values.val_datetime == toml.DateTime{'2024-10-05 09:10:20.000'}
	assert test_has_values.val_date == toml.Date{'2099-09-09'}
	assert test_has_values.val_time == toml.Time{'22:22:22.222'}

	toml_all_default_values := toml.parse_text(toml_all_default_text) or { panic(err) }
	test_all_default_values := toml_all_default_values.reflect[Test]()

	assert test_all_default_values.val_string == 'abcd'
	assert test_all_default_values.val_bool == true
	assert test_all_default_values.val_int == 123
	assert test_all_default_values.val_i64 == 1234
	assert test_all_default_values.val_u64 == 12345
	assert test_all_default_values.val_f32 == 100.1
	assert test_all_default_values.val_f64 == 1000.1
	assert test_all_default_values.val_datetime == toml.DateTime{'1980-07-11 21:23:42.123'}
	assert test_all_default_values.val_date == toml.Date{'1977-07-07'}
	assert test_all_default_values.val_time == toml.Time{'11:11:11.111'}
}

fn test_decode_struct_default_values() {
	test_has_values := toml.decode[Test](toml_text) or { panic(err) }
	assert test_has_values.val_string == 'test'
	assert test_has_values.val_bool == false
	assert test_has_values.val_int == 456
	assert test_has_values.val_i64 == 4567
	assert test_has_values.val_u64 == 45678
	assert test_has_values.val_f32 == 200.2
	assert test_has_values.val_f64 == 2000.2
	assert test_has_values.val_datetime == toml.DateTime{'2024-10-05 09:10:20.000'}
	assert test_has_values.val_date == toml.Date{'2099-09-09'}
	assert test_has_values.val_time == toml.Time{'22:22:22.222'}

	test_all_default_values := toml.decode[Test](toml_all_default_text) or { panic(err) }
	assert test_all_default_values.val_string == 'abcd'
	assert test_all_default_values.val_bool == true
	assert test_all_default_values.val_int == 123
	assert test_all_default_values.val_i64 == 1234
	assert test_all_default_values.val_u64 == 12345
	assert test_all_default_values.val_f32 == 100.1
	assert test_all_default_values.val_f64 == 1000.1
	assert test_all_default_values.val_datetime == toml.DateTime{'1980-07-11 21:23:42.123'}
	assert test_all_default_values.val_date == toml.Date{'1977-07-07'}
	assert test_all_default_values.val_time == toml.Time{'11:11:11.111'}
}
