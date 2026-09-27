@[translated]
module main

enum TranslatedCode {
	zero
	one
}

fn translated_number(value int) int {
	return value
}

fn translated_enum_result() int {
	return TranslatedCode.one
}

fn test_translated_scalar_conversions() {
	value := int(0)
	value = TranslatedCode.one
	assert value == 1
	flag := char(0)
	assert (if !flag { 7 } else { 0 }) == 7
	flag = 1
	assert (if flag && value { 7 } else { 0 }) == 7
	flag = `z`
	assert flag == `z`
	value = true
	assert value == 1
	assert translated_number(TranslatedCode.one) == 1
	assert translated_number(true) == 1
	assert translated_enum_result() == 1
	assert value == TranslatedCode.one
	wide := i64(17)
	assert translated_number(wide) == 17
	floating := f64(0)
	floating = wide
	assert floating == 17.0
}

fn test_translated_conditions() {
	mut remaining := 3
	mut count := 0
	for remaining {
		count++
		remaining--
	}
	assert count == 3
	assert (if TranslatedCode.one && TranslatedCode.zero { 1 } else { 0 }) == 0
	assert (if TranslatedCode.zero || TranslatedCode.one { 1 } else { 0 }) == 1
	mut value := 5
	pointer := &value
	assert (if pointer { 1 } else { 0 }) == 1
	assert !(!pointer)
	assert (if pointer && value { 1 } else { 0 }) == 1
}

fn test_translated_enum_arithmetic() {
	value := 11
	assert value - TranslatedCode.one == 10
	assert value + TranslatedCode.one == 12
	assert TranslatedCode.one * 4 == 4
	assert TranslatedCode.one / 1 == 1
	assert value % TranslatedCode.one == 0
	assert ((value - TranslatedCode.one) / 5) << 3 == 16
	assert f64(2.5) + TranslatedCode.one == 3.5
	assert char(2) + value == 13
}
