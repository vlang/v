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

type TranslatedCallback = fn ()

struct TranslatedCallbacks {
	callback TranslatedCallback = unsafe { nil }
}

fn test_translated_compound_and_bitwise_operators() {
	mut value := 10
	value += true
	value -= TranslatedCode.one
	value *= TranslatedCode.one
	assert value == 10
	value ^= true
	assert value == 11
	assert (3 ^ true) == 2
	masked := char(3) & 1
	assert int(masked) == 1
	assert int(TranslatedCode.one | TranslatedCode.zero) == 1
	callbacks := TranslatedCallbacks{}
	assert (if !callbacks.callback { 1 } else { 0 }) == 1
}

fn test_translated_scalar_postfix_mutations() {
	mut state := TranslatedCode.zero
	state++
	assert state == TranslatedCode.one
	state--
	assert state == TranslatedCode.zero
	mut character := char(0)
	character++
	assert character == 1
	character--
	assert character == 0
	mut flag := false
	flag++
	assert flag
	flag++
	assert flag
	flag--
	assert !flag
	flag--
	assert flag
	mut flags := [false]
	mut evaluations := [0]
	flags[translated_postfix_index(mut evaluations)]++
	assert flags[0]
	assert evaluations[0] == 1
	mut states := {
		'key': TranslatedCode.zero
	}
	states['key']++
	assert states['key'] == TranslatedCode.one
}

type TranslatedFlag = bool

fn translated_flag_arg(value bool) bool { return value }

fn translated_flag_return(value f64) bool { return value }

struct TranslatedFlagHolder {
mut:
	flag bool
}

fn translated_flag_mut(mut value TranslatedFlagHolder) {
	value.flag += 256
}

fn test_translated_boolean_destinations_normalize_nonzero_values() {
	mut flag := false
	flag = 256
	assert flag == true
	flag = 0.5
	assert flag == true
	flag = -0.5
	assert flag == true
	flag = 0
	assert flag == false
	assert translated_flag_arg(256) == true
	assert translated_flag_arg(0.5) == true
	assert translated_flag_return(0.5) == true
	assert translated_flag_return(0.0) == false
	// Explicit numeric-to-bool casts require unsafe even in translated code.
	assert unsafe { bool(256) } == true
	assert unsafe { bool(0.5) } == true
	mut alias_flag := unsafe { TranslatedFlag(0.5) }
	assert alias_flag == true
	alias_flag = 256
	assert alias_flag == true
	flag += 256
	assert flag == true
	flag *= 0.5
	assert flag == true
	flag -= 1
	assert flag == false
	mut holder := TranslatedFlagHolder{ flag: 0.5 }
	assert holder.flag == true
	translated_flag_mut(mut holder)
	assert holder.flag == true
	flag = true
	flag <<= 1
	assert flag == true
	flag >>= 1
	assert flag == false
	flag |= 256
	assert flag == true
	mut flags := [false, false]
	mut evaluations := [0]
	flags[translated_postfix_index(mut evaluations)] += 256
	assert evaluations == [1]
	assert flags[0] == true
	flags[0] <<= 1
	assert flags[0] == true
	mut fixed := [false]!
	fixed[0] = 256
	fixed[0] <<= 1
	assert fixed[0] == true
	mut entries := {
		'flag': false
	}
	entries['flag'] += 256
	assert entries['flag'] == true
}

fn translated_postfix_index(mut evaluations []int) int {
	evaluations[0]++
	return 0
}

fn translated_postfix_key(mut evaluations []int) string {
	evaluations[0]++
	return 'flag'
}

fn test_translated_boolean_map_postfix_stays_normalized() {
	mut flags := {
		'flag': true
	}
	mut evaluations := [0]
	flags[translated_postfix_key(mut evaluations)]++
	assert evaluations == [1]
	assert flags['flag'] == true
	flags['flag']++
	assert flags['flag'] == true
	flags['flag']--
	assert flags['flag'] == false
	flags['missing']--
	assert flags['missing'] == true
	before := flags['flag']++
	assert before == false
	assert flags['flag'] == true
	mut aliases := {
		'flag': TranslatedFlag(true)
	}
	aliases['flag']++
	assert aliases['flag'] == true
	mut nested := {
		'row': {
			'flag': true
		}
	}
	nested['row']['flag']++
	assert nested['row']['flag'] == true
	nested['row']['flag']--
	assert nested['row']['flag'] == false
	nested['row']['missing']--
	assert nested['row']['missing'] == true
}
