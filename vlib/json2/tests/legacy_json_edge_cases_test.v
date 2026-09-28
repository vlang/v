import json2
import time

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

enum NullColor {
	red
	green
}

struct NullFields {
	i int
	d int = 5
	b bool
	t bool      = true
	f f64       = 1.5
	s string    = 'x'
	c NullColor = .green
}

fn test_null_decodes_to_the_zero_value() {
	fields := json2.decode[NullFields]('{"i":null,"d":null,"b":null,"t":null,"f":null,"s":null,"c":null}')!
	assert fields == NullFields{
		i: 0
		d: 0
		b: false
		t: false
		f: 0.0
		s: ''
		c: .red
	}
	assert json2.decode[[]int]('[1,null,3]')! == [1, 0, 3]
	assert json2.decode[map[string]bool]('{"a":null}')! == {
		'a': false
	}
	// Strict mode keeps rejecting `null` for a value that is not an option.
	if _ := json2.decode[[]int]('[null]', strict: true) {
		assert false
	}
}

fn test_multi_pointer_top_level_targets() {
	assert **json2.decode[&&int]('5')! == 5
	assert (***json2.decode[&&&Human]('{"name":"p"}')!).name == 'p'
}

type Timestamp = time.Time

type TimeValue = Timestamp | int

struct TimeValueHolder {
	value TimeValue
}

fn test_time_alias_sumtype_variant() {
	holder := TimeValueHolder{
		value: TimeValue(Timestamp(time.unix(1608621780)))
	}
	encoded := json2.encode(holder, time_as_unix: true)
	// Like the removed module, every time variant is written as `Time`.
	assert encoded == '{"value":{"_type":"Time","value":1608621780}}'
	decoded := json2.decode[TimeValueHolder](encoded)!
	assert decoded.value.type_name() == 'Timestamp'
	// The variant's own name is accepted as well.
	by_name := json2.decode[TimeValueHolder]('{"value":{"_type":"Timestamp","value":7}}')!
	assert by_name.value.type_name() == 'Timestamp'
}

struct NullInner {
	a int = 3
}

struct NullContainers {
	list  []int
	m     map[string]int
	inner NullInner
	at    time.Time
	lists [][]int
	objs  []NullInner
}

fn test_null_containers_structs_and_times() {
	assert json2.decode[[]int]('null')! == []int{}
	assert json2.decode[map[string]int]('null')!.len == 0
	assert json2.decode[time.Time]('null')! == time.Time{}
	decoded := json2.decode[NullContainers]('{"list":null,"m":null,"inner":null,"at":null,"lists":[null,[1]],"objs":[null]}')!
	assert decoded.list == []
	assert decoded.m.len == 0
	assert decoded.inner.a == 3
	assert decoded.at == time.Time{}
	assert decoded.lists == [[]int{}, [1]]
	assert decoded.objs.len == 1
	assert decoded.objs[0].a == 3
	// Like the removed module, a `null` root is not a struct.
	if _ := json2.decode[NullInner]('null') {
		assert false
	}
	// Strict mode keeps rejecting these.
	if _ := json2.decode[[]int]('null', strict: true) {
		assert false
	}
	if _ := json2.decode[NullContainers]('{"inner":null}', strict: true) {
		assert false
	}
}

struct NestedCat {
	name string
}

struct NestedDog {
	name string
}

type NestedPet = NestedCat | NestedDog

struct NestedOwner {
	pet NestedPet
}

struct NestedShop {
	title string
}

type NestedPlace = NestedOwner | NestedShop

fn test_nested_sumtype_round_trip() {
	place := NestedPlace(NestedOwner{
		pet: NestedPet(NestedDog{'rex'})
	})
	encoded := json2.encode(place)
	// The inner `_type` comes first, and must not be taken for the outer one.
	assert encoded == '{"pet":{"name":"rex","_type":"NestedDog"},"_type":"NestedOwner"}'
	decoded := json2.decode[NestedPlace](encoded)!
	owner := decoded as NestedOwner
	assert (owner.pet as NestedDog).name == 'rex'
}

struct InnerNilPointers {
	a &&int
	b &&&int
}

fn test_encode_inner_nil_pointers() {
	inner := &int(unsafe { nil })
	middle := &&int(unsafe { nil })
	value := InnerNilPointers{
		a: &inner
		b: &middle
	}
	assert json2.encode(value) == '{"a":null,"b":null}'
}

fn test_escaped_sumtype_discriminator_key() {
	being := json2.decode[Being]('{"_\\u0074ype":"Human","name":"x"}')!
	assert being is Human
	assert (being as Human).name == 'x'
	robot := json2.decode[Being]('{"_typ\\u0065":"Robot","model":"r2"}')!
	assert robot is Robot
}

struct OmitInner {
	a int
}

struct OmitDefault {
	a int = 3
}

type OmitValue = OmitInner | int

struct OmitHolder {
	c NullColor   @[omitempty]
	i OmitInner   @[omitempty]
	w OmitDefault @[omitempty]
	v OmitValue   @[omitempty]
}

fn test_omitempty_of_enums_structs_and_sumtypes() {
	// Like the removed module: a field equal to its type's default value is omitted.
	assert json2.encode(OmitHolder{}) == '{}'
	assert json2.encode(OmitHolder{
		c: .green
		i: OmitInner{1}
		w: OmitDefault{0}
		v: OmitValue(5)
	}) == '{"c":"green","i":{"a":1},"w":{"a":0},"v":5}'
	assert json2.encode(OmitHolder{ v: OmitValue(0) }) == '{"v":0}'
}

struct StringTargets {
	s  string
	os ?string
	ls []string
	ms map[string]string
}

fn test_objects_and_arrays_decode_into_strings() {
	assert json2.decode[[]string]('[{"a":1},[1,2],"x"]')! == ['{"a":1}', '[1,2]', 'x']
	assert json2.decode[map[string]string]('{"a":{"b":2}}')! == {
		'a': '{"b":2}'
	}
	// A string root still has to be a JSON string, as the old module had no string root.
	if _ := json2.decode[string]('[1]') {
		assert false
	}
	targets := json2.decode[StringTargets]('{"s":{"k":1},"os":[1],"ls":[{"z":1}],"ms":{"q":[2]}}')!
	assert targets.s == '{"k":1}'
	assert targets.os? == '[1]'
	assert targets.ls == ['{"z":1}']
	assert targets.ms['q'] == '[2]'
}

struct RequiredName {
	name string @[required]
}

struct RequiredList {
	list []int @[required]
}

fn test_required_fields_reject_null() {
	if _ := json2.decode[RequiredName]('{"name":null}') {
		assert false
	}
	if _ := json2.decode[RequiredList]('{"list":null}') {
		assert false
	}
	assert json2.decode[RequiredName]('{"name":"x"}')!.name == 'x'
}

type OptionVariant = ?int | string

struct OptionFoo {
	a int
}

type OptionStructVariant = ?OptionFoo | int

fn test_option_sumtype_variants_with_values() {
	number := json2.decode[OptionVariant]('5')!
	assert json2.encode(number) == '5'
	text := json2.decode[OptionVariant]('"s"')!
	assert text == OptionVariant('s')
	nothing := json2.decode[OptionVariant]('null')!
	assert json2.encode(nothing) == '{}'
	foo := OptionStructVariant(?OptionFoo(OptionFoo{2}))
	encoded := json2.encode(foo)
	assert encoded == '{"a":2,"_type":"OptionFoo"}'
	assert json2.encode(json2.decode[OptionStructVariant](encoded)!) == encoded
}
