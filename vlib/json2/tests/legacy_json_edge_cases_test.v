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

struct OptionRunes {
	list  []?rune
	fixed [2]?rune
	by_id map[string]?rune
}

fn test_option_rune_elements() {
	runes := json2.decode[OptionRunes]('{"list":["q",null],"fixed":[null,"d"],"by_id":{"k":"e"}}')!
	first := runes.list[0] or { panic('the first rune should be set') }
	assert first == `q`
	assert runes.list[1] == none
	assert runes.fixed[0] == none
	second := runes.fixed[1] or { panic('the second rune should be set') }
	assert second == `d`
	by_id := runes.by_id['k'] or { panic('k should be set') }
	assert by_id == `e`
	top := json2.decode[[]?rune]('["z"]')!
	z := top[0] or { panic('z should be set') }
	assert z == `z`
}

type OptionTimeVariant = ?time.Time | int

struct OptionFooVariant {
	a int
}

struct OptionBarVariant {
	b int
}

type OptionStructs = ?OptionFooVariant | ?OptionBarVariant

fn test_option_time_and_struct_variants() {
	value := OptionTimeVariant(?time.Time(time.unix(100)))
	encoded := json2.encode(value)
	// Like the removed module, a time in an option variant is a `Time` object.
	assert encoded == '{"_type":"Time","value":100}'
	decoded := json2.decode[OptionTimeVariant](encoded)!
	decoded_time := (decoded as ?time.Time) or { panic('the time should be set') }
	assert decoded_time.unix() == 100
	// `_type` selects between option variants of structs.
	bar := json2.decode[OptionStructs]('{"b":2,"_type":"OptionBarVariant"}')!
	assert json2.encode(bar) == '{"b":2,"_type":"OptionBarVariant"}'
	foo := json2.decode[OptionStructs]('{"a":1,"_type":"OptionFooVariant"}')!
	assert json2.encode(foo) == '{"a":1,"_type":"OptionFooVariant"}'
}

struct OmitemptyConfig {
	retries int    = 3   @[omitempty]
	name    string = 'x'   @[omitempty]
	ratio   f64    = 1.5 @[omitempty]
}

fn test_omitempty_fields_decode_explicit_empty_values() {
	// Like the removed module, `omitempty` only affects encoding.
	config := json2.decode[OmitemptyConfig]('{"retries":0,"name":"","ratio":0.0}')!
	assert config.retries == 0
	assert config.name == ''
	assert config.ratio == 0.0
	defaults := json2.decode[OmitemptyConfig]('{}')!
	assert defaults.retries == 3
	assert defaults.name == 'x'
	assert defaults.ratio == 1.5
	assert json2.encode(OmitemptyConfig{ retries: 0, name: '', ratio: 0.0 }) == '{}'
}

struct HookedValue {
mut:
	a int
}

fn (h HookedValue) to_json() string {
	return '"custom"'
}

fn (mut h HookedValue) from_json_string(raw string) ! {
	h.a = raw.len
}

struct HookedHolder {
	value HookedValue
}

fn test_custom_hooks_encode_through_to_json_and_decode_legacy_objects() {
	// Unlike the removed module, which wrote `{"value":{"a":1}}`, json2 encodes a type
	// through its own `to_json()`; the json2 README documents this difference.
	assert json2.encode(HookedHolder{ value: HookedValue{ a: 1 } },
		escape_unicode: true
		time_as_unix:   true
	) == '{"value":"custom"}'
	// Objects written by the removed module still decode field by field, since the
	// `from_json_string` hook only handles JSON strings.
	assert json2.decode[HookedHolder]('{"value":{"a":2}}')!.value.a == 2
	assert json2.decode[HookedValue]('"xyz"')!.a == 3
}

type OptionText = string

type OptionTextValue = ?int | ?OptionText

type OptionTimestamp = time.Time

type OptionTimestampValue = ?OptionTimestamp | int

type OptionNums = []int

type OptionNumsValue = ?int | ?OptionNums

fn test_option_variants_of_aliases() {
	// The unaliased payload selects the option variant: a string goes to `?OptionText`.
	text_value := json2.decode[OptionTextValue]('"hello"')!
	text := (text_value as ?OptionText) or { panic('the text should be set') }
	assert text == 'hello'
	int_value := json2.decode[OptionTextValue]('5')!
	number := (int_value as ?int) or { panic('the int should be set') }
	assert number == 5
	nums_value := json2.decode[OptionNumsValue]('[1,2]')!
	nums := (nums_value as ?OptionNums) or { panic('the array should be set') }
	assert nums == OptionNums([1, 2])
	// An option of a time alias is written with the `Time` discriminator, and read back.
	stamp := OptionTimestampValue(?OptionTimestamp(OptionTimestamp(time.unix(100))))
	encoded := json2.encode(stamp, time_as_unix: true)
	assert encoded == '{"_type":"Time","value":100}'
	decoded := json2.decode[OptionTimestampValue](encoded)!
	decoded_stamp := (decoded as ?OptionTimestamp) or { panic('the time should be set') }
	assert time.Time(decoded_stamp).unix() == 100
}

struct ArrayVariantItem {
	a int
}

type NestedItems = [][]ArrayVariantItem | int

type FixedItems = [2]ArrayVariantItem | int

type NestedFixedItems = [][2]ArrayVariantItem | int

fn test_nested_and_fixed_array_variants_tag_struct_elements() {
	// Like the removed module, struct elements get their `_type` at every array level.
	nested := NestedItems([[ArrayVariantItem{1}], [ArrayVariantItem{2}]])
	nested_json := json2.encode(nested)
	assert nested_json == '[[{"a":1,"_type":"ArrayVariantItem"}],[{"a":2,"_type":"ArrayVariantItem"}]]'
	assert json2.decode[NestedItems](nested_json)! == nested
	fixed := FixedItems([ArrayVariantItem{1}, ArrayVariantItem{2}]!)
	fixed_json := json2.encode(fixed)
	assert fixed_json == '[{"a":1,"_type":"ArrayVariantItem"},{"a":2,"_type":"ArrayVariantItem"}]'
	assert json2.decode[FixedItems](fixed_json)! == fixed
	nested_fixed := NestedFixedItems([[ArrayVariantItem{1}, ArrayVariantItem{2}]!])
	nested_fixed_json := json2.encode(nested_fixed)
	assert nested_fixed_json == '[[{"a":1,"_type":"ArrayVariantItem"},{"a":2,"_type":"ArrayVariantItem"}]]'
	assert json2.decode[NestedFixedItems](nested_fixed_json)! == nested_fixed
	assert json2.encode(nested, prettify: true, legacy_layout: true) == '[[{\n\t\t\t"a":\t1,\n\t\t\t"_type":\t"ArrayVariantItem"\n\t\t}], [{\n\t\t\t"a":\t2,\n\t\t\t"_type":\t"ArrayVariantItem"\n\t\t}]]'
}

struct RequiredRawBody {
	body string @[raw; required]
}

fn test_required_raw_field_keeps_null_text() {
	// The removed module kept a `null` of a `@[raw]` field as its text, also with
	// `@[required]`.
	assert json2.decode[RequiredRawBody]('{"body":null}')!.body == 'null'
}

type IntOrRune = ?int | ?rune

type RuneOrString = ?rune | ?string

type RuneOrInt = ?rune | ?int

fn test_option_rune_variants_take_strings() {
	// A rune is written as a string, so `?rune` takes one when no `?string` does.
	encoded := json2.encode(IntOrRune(?rune(`q`)))
	assert encoded == '"q"'
	decoded := json2.decode[IntOrRune](encoded)!
	letter := (decoded as ?rune) or { panic('the rune should be set') }
	assert letter == `q`
	int_value := json2.decode[IntOrRune]('5')!
	number := (int_value as ?int) or { panic('the int should be set') }
	assert number == 5
	// A payload of the JSON value's own kind wins over one that converts it.
	string_value := json2.decode[RuneOrString]('"hello"')!
	text := (string_value as ?string) or { panic('the string should be set') }
	assert text == 'hello'
	count_value := json2.decode[RuneOrInt]('5')!
	count := (count_value as ?int) or { panic('the int should be set') }
	assert count == 5
}

type OptionItems = ?[]ArrayVariantItem | int

type OptionFixedItems = ?[2]ArrayVariantItem | int

type OptionNestedItems = ?[][]ArrayVariantItem | int

fn test_option_array_variants_tag_struct_elements() {
	// Like the removed module, struct elements in an option variant's array get `_type`.
	items := OptionItems(?[]ArrayVariantItem([ArrayVariantItem{1}]))
	items_json := json2.encode(items)
	assert items_json == '[{"a":1,"_type":"ArrayVariantItem"}]'
	decoded_items := json2.decode[OptionItems](items_json)!
	decoded_list := (decoded_items as ?[]ArrayVariantItem) or { panic('the array should be set') }
	assert decoded_list == [ArrayVariantItem{1}]
	fixed := OptionFixedItems(?[2]ArrayVariantItem([ArrayVariantItem{1}, ArrayVariantItem{2}]!))
	assert json2.encode(fixed) == '[{"a":1,"_type":"ArrayVariantItem"},{"a":2,"_type":"ArrayVariantItem"}]'
	nested := OptionNestedItems(?[][]ArrayVariantItem([[ArrayVariantItem{1}]]))
	assert json2.encode(nested) == '[[{"a":1,"_type":"ArrayVariantItem"}]]'
}

struct SumRefHolder {
	value  &Being
	values []&Being
	maybe  ?&Being
}

fn test_references_to_sum_types_decode() {
	// Like the removed module, a `&SumType` field decodes into a new sum type value.
	input := '{"value":{"name":"Bob","_type":"Human"},"values":[{"model":"R2","_type":"Robot"}],"maybe":{"name":"Al","_type":"Human"}}'
	holder := json2.decode[SumRefHolder](input)!
	assert json2.encode(holder) == input
	value := *holder.value
	assert value is Human
	top := json2.decode[&Being]('{"model":"C3","_type":"Robot"}')!
	assert json2.encode(top) == '{"model":"C3","_type":"Robot"}'
}

type AliasItem = ArrayVariantItem

type AliasItemValue = AliasItem | int

type OptionAliasItemValue = ?AliasItem | int

type AliasItemsValue = []AliasItem | int

type ItemOrAlias = ArrayVariantItem | AliasItem

fn test_struct_alias_variants_use_the_struct_name() {
	// Like the removed module, an alias of a struct is tagged with the struct's name.
	value := AliasItemValue(AliasItem(ArrayVariantItem{1}))
	assert json2.encode(value) == '{"a":1,"_type":"ArrayVariantItem"}'
	option_value := OptionAliasItemValue(?AliasItem(AliasItem(ArrayVariantItem{2})))
	assert json2.encode(option_value) == '{"a":2,"_type":"ArrayVariantItem"}'
	items := AliasItemsValue([AliasItem(ArrayVariantItem{3})])
	assert json2.encode(items) == '[{"a":3,"_type":"ArrayVariantItem"}]'
	// Both the struct's name and the alias's own name are read back.
	for tag in ['ArrayVariantItem', 'AliasItem'] {
		decoded := json2.decode[AliasItemValue]('{"a":4,"_type":"${tag}"}')!
		assert decoded.type_name() == 'AliasItem'
		assert json2.encode(decoded) == '{"a":4,"_type":"ArrayVariantItem"}'
		decoded_option := json2.decode[OptionAliasItemValue]('{"a":5,"_type":"${tag}"}')!
		assert json2.encode(decoded_option) == '{"a":5,"_type":"ArrayVariantItem"}'
		decoded_items := json2.decode[AliasItemsValue]('[{"a":6,"_type":"${tag}"}]')!
		assert json2.encode(decoded_items) == '[{"a":6,"_type":"ArrayVariantItem"}]'
	}
	// The exact name wins when a sum type holds both the struct and its alias.
	by_struct := json2.decode[ItemOrAlias]('{"a":7,"_type":"ArrayVariantItem"}')!
	assert by_struct.type_name() == 'ArrayVariantItem'
	by_alias := json2.decode[ItemOrAlias]('{"a":8,"_type":"AliasItem"}')!
	assert by_alias.type_name() == 'AliasItem'
}

type TimesValue = []time.Time | int

type FixedTimesValue = [2]time.Time | int

fn test_time_elements_of_array_variants_keep_the_time_wrapper() {
	// Like the removed module, a time in an array variant is a `Time` object.
	times := TimesValue([time.unix(123)])
	encoded := json2.encode(times, time_as_unix: true)
	assert encoded == '[{"_type":"Time","value":123}]'
	decoded := json2.decode[TimesValue](encoded)!
	decoded_times := decoded as []time.Time
	assert decoded_times[0].unix() == 123
	fixed := FixedTimesValue([time.unix(1), time.unix(2)]!)
	assert json2.encode(fixed) == '[{"_type":"Time","value":1},{"_type":"Time","value":2}]'
	// A time field also reads the wrapper written by the removed module.
	assert json2.decode[time.Time]('{"_type":"Time","value":5}')!.unix() == 5
}

fn test_time_wrapper_needs_the_time_discriminator() {
	// Only the `Time` wrapper is a time; other objects are rejected, like before.
	assert json2.decode[time.Time]('{"_type":"Ti\\u006de","value":7}')!.unix() == 7
	if _ := json2.decode[time.Time]('{"_type":"Robot","value":123}') {
		assert false, 'a mistagged object should not decode as a time'
	}
	if _ := json2.decode[time.Time]('{"value":123}') {
		assert false, 'an untagged object should not decode as a time'
	}
	if _ := json2.decode[[]time.Time]('[{"_type":"Time","value":1},{"_type":"Robot","value":2}]') {
		assert false, 'a mistagged element should not decode as a time'
	}
}

type OptionItemsArray = []?ArrayVariantItem | int

type FixedOptionItems = [2]?ArrayVariantItem | int

type OptionIntsArray = []?int | string

fn test_option_elements_of_array_variants() {
	// Like the removed module, a set element is written like the variant it holds, with
	// its `_type`, and a `none` element as `{}`.
	items := OptionItemsArray([?ArrayVariantItem(ArrayVariantItem{1}), none])
	encoded := json2.encode(items)
	assert encoded == '[{"a":1,"_type":"ArrayVariantItem"},{}]'
	fixed := FixedOptionItems([?ArrayVariantItem(ArrayVariantItem{2}), none]!)
	assert json2.encode(fixed) == '[{"a":2,"_type":"ArrayVariantItem"},{}]'
	ints := OptionIntsArray([?int(5), none])
	ints_json := json2.encode(ints)
	assert ints_json == '[5,{}]'
	// The variant is resolved from these elements, also from a leading `{}`.
	decoded := json2.decode[OptionItemsArray](encoded)!
	decoded_items := decoded as []?ArrayVariantItem
	first := decoded_items[0] or { panic('the first element should be set') }
	assert first.a == 1
	leading := json2.decode[OptionItemsArray]('[{},{"a":3,"_type":"ArrayVariantItem"}]')!
	assert (leading as []?ArrayVariantItem).len == 2
	// `{}` is `none` for a payload that cannot be an object.
	decoded_ints := json2.decode[OptionIntsArray](ints_json)!
	int_items := decoded_ints as []?int
	five := int_items[0] or { panic('the first int should be set') }
	assert five == 5
	assert int_items[1] == none
}
