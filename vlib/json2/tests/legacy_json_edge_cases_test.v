import json2

// Inputs that the removed `json` module accepted, and that json2 has to handle the
// same way for migrated code.

@[json_as_number]
enum Status {
	ok   = 1
	fail = 2
}

enum Plain {
	one = 1
	two = 2
}

struct Human {
	name string
}

struct Robot {
	model string
}

type Being = Human | Robot

fn test_encode_malformed_utf8_with_escape_unicode() {
	// Every invalid or truncated byte becomes U+FFFD instead of slicing past the end.
	assert json2.encode([u8(0xff)].bytestr(), escape_unicode: true) == '"\\ufffd"'
	assert json2.encode([u8(0xe2), 0x82].bytestr(), escape_unicode: true) == '"\\ufffd\\ufffd"'
	assert json2.encode([u8(0xe2), 0x28, 0xa1].bytestr(), escape_unicode: true) == '"\\ufffd(\\ufffd"'
	assert json2.encode([u8(0xf0), 0x9f, 0x98].bytestr(), escape_unicode: true) == '"\\ufffd\\ufffd\\ufffd"'
	assert json2.encode('aé😀', escape_unicode: true) == '"a\\u00e9\\uD83D\\ude00"'
	// Without escaping, the bytes are written as they are.
	assert json2.encode([u8(0xff)].bytestr()) == '"' + [u8(0xff)].bytestr() + '"'
}

fn test_json_as_number_enum_keeps_undeclared_values() {
	assert json2.decode[Status]('2')! == .fail
	undeclared := json2.decode[Status]('99')!
	assert int(undeclared) == 99
	encoded := json2.encode(undeclared)
	assert encoded == '99'
	assert int(json2.decode[Status](encoded)!) == 99
	// A plain enum still has to name a declared value.
	if _ := json2.decode[Plain]('99') {
		assert false
	}
}

fn test_escaped_sumtype_discriminator() {
	being := json2.decode[Being]('{"_type":"Hum\\u0061n","name":"x"}')!
	assert being is Human
	assert (being as Human).name == 'x'
	robot := json2.decode[Being]('{"_type":"Robot","model":"r2"}')!
	assert robot is Robot
}

@[json_as_number]
enum Wide as u64 {
	low  = 1
	high = 9223372036854775808
}

@[json_as_number]
enum WideSigned as i64 {
	neg = -9000000000
}

struct OptionContainers {
	fixed  [2]?int
	by_key map[string]?int
	humans []?Human
}

fn test_option_elements_in_containers() {
	assert json2.decode[[]?int]('[1,null]')! == [?int(1), none]
	containers := json2.decode[OptionContainers]('{"fixed":[null,2],"by_key":{"x":null,"y":5},"humans":[{"name":"h"},null]}')!
	assert containers.fixed[0] == none
	second := containers.fixed[1]
	assert second? == 2
	assert containers.by_key['x'] == none
	y := containers.by_key['y']
	assert y? == 5
	first_human := containers.humans[0] or { panic('the first human should be set') }
	assert first_human.name == 'h'
	assert containers.humans[1] == none
}

fn test_json_as_number_enum_uses_the_backing_type() {
	assert json2.decode[Wide]('9223372036854775808')! == .high
	assert json2.encode(Wide.high) == '9223372036854775808'
	assert json2.decode[WideSigned]('-9000000000')! == .neg
	assert json2.encode(WideSigned.neg) == '-9000000000'
}
