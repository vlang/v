import json2 as json

// Non-JSON metadata must be valid while exercising the decoder's attribute filter.
@[attribute]
struct Badattr {
	value string
}

@[attribute]
struct Foobar {}

enum Bar {
	a
	b  @[json: 'BBB']
	c = 10
	e  @[Badattr: 'bad'; Foobar]
}

type BarAlias = Bar

fn test_number_decode() {
	assert json.decode[Bar]('0')! == Bar.a
	assert json.decode[Bar]('1')! == Bar.b
	assert json.decode[Bar]('10')! == Bar.c

	assert json.decode[BarAlias]('0')! == Bar.a
	assert json.decode[BarAlias]('1')! == Bar.b
	assert json.decode[BarAlias]('10')! == Bar.c
}

fn test_number_decode_fails() {
	if _ := json.decode[Bar]('2') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Number value: `2` does not match any field in enum: &Bar'
		}
	}

	if _ := json.decode[BarAlias]('2') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Number value: `2` does not match any field in enum: &BarAlias'
		}
	}
}

fn test_string_decode() {
	assert json.decode[Bar]('"a"')! == Bar.a
	assert json.decode[Bar]('"BBB"')! == Bar.b
	assert json.decode[Bar]('"c"')! == Bar.c

	assert json.decode[BarAlias]('"a"')! == Bar.a
	assert json.decode[BarAlias]('"BBB"')! == Bar.b
	assert json.decode[BarAlias]('"c"')! == Bar.c
}

fn test_string_decode_uses_only_json_attr() {
	assert json.decode[Bar]('"e"')! == Bar.e
	assert json.decode[BarAlias]('"e"')! == Bar.e
	for badval in ['"Badattr: bad"', '"bad"', '"Foobar"', '"badattr: bad"', '"foobar"'] {
		if _ := json.decode[Bar](badval) {
			assert false, '${badval} must not be decoded to Bar'
		} else {
			assert true
		}
		if _ := json.decode[BarAlias](badval) {
			assert false, '${badval} must not be decoded to BarAlias'
		} else {
			assert true
		}
	}
}

fn test_string_decode_fails() {
	if _ := json.decode[Bar]('"d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: String value: `d` does not match any field in enum: &Bar'
		}
	}

	if _ := json.decode[BarAlias]('"d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: String value: `d` does not match any field in enum: &BarAlias'
		}
	}
}

fn test_invalid_decode_fails() {
	if _ := json.decode[Bar]('true') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected number or string value for enum, got: boolean'
		}
	}

	if _ := json.decode[BarAlias]('true') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected number or string value for enum, got: boolean'
		}
	}
}

fn test_map_string_enum_decode() {
	m := json.decode[map[string]Bar]('{"foo": "a", "bar": "b", "baz": "c"}')!
	assert m['foo'] == .a
	assert m['bar'] == .b
	assert m['baz'] == .c

	m2 := json.decode[map[string]Bar]('{"x": 0, "y": 1, "z": 10}')!
	assert m2['x'] == .a
	assert m2['y'] == .b
	assert m2['z'] == .c
}

enum MapKey {
	first
	second = 7
}

type AliasMapKey = MapKey

@[flag]
enum Permission {
	read
	write
	execute
}

type AliasPermission = Permission

struct EnumMapHolder {
	entries map[MapKey]int
}

fn test_decode_enum_map_keys() {
	decoded := json.decode[map[MapKey]int]('{"first":11,"second":27}')!
	assert decoded[.first] == 11
	assert decoded[.second] == 27
}

fn test_enum_map_keys_round_trip_in_struct() {
	original := EnumMapHolder{
		entries: {
			MapKey.second: 42
		}
	}
	decoded := json.decode[EnumMapHolder](json.encode(original))!
	assert decoded == original
}

fn test_decode_alias_enum_map_key() {
	decoded := json.decode[map[AliasMapKey]int]('{"second":42}')!
	assert decoded[AliasMapKey(MapKey.second)] == 42
}

fn test_flag_enum_map_keys_round_trip() {
	for permission in [Permission.read, Permission.write, Permission.read | Permission.write,
		Permission.read & Permission.write] {
		original := {
			permission: 7
		}
		encoded := json.encode(original)
		decoded := json.decode[map[Permission]int](encoded)!
		assert decoded == original, encoded
	}
}

fn test_alias_flag_enum_map_keys_round_trip() {
	permission := AliasPermission(Permission.read | Permission.write)
	original := {
		permission: 7
	}
	decoded := json.decode[map[AliasPermission]int](json.encode(original))!
	assert decoded == original
}

fn test_flag_enum_map_keys_reject_invalid_values() {
	for key in ['Permission{.missing}', 'Permission{.read | .missing}', 'Other{.read}',
		'Permission{.read', 'Permission{read}', 'Permission{.read | }'] {
		json.decode[map[Permission]int]('{"${key}":7}') or {
			assert err.msg().contains(key)
			continue
		}
		assert false, 'invalid flag key was accepted: ${key}'
	}
}

fn test_flag_enum_map_single_keys_round_trip() {
	original := {
		Permission.read:  7
		Permission.write: 11
	}
	decoded := json.decode[map[Permission]int](json.encode(original))!
	assert decoded == original
}

fn test_flag_enum_map_combined_keys_round_trip() {
	original := {
		Permission.read | Permission.write:   13
		Permission.read | Permission.execute: 17
	}
	decoded := json.decode[map[Permission]int](json.encode(original))!
	assert decoded == original
}

fn test_flag_enum_map_zero_key_round_trip() {
	original := {
		unsafe { Permission(0) }: 19
	}
	decoded := json.decode[map[Permission]int](json.encode(original))!
	assert decoded == original
}

fn test_flag_enum_map_alias_keys_round_trip() {
	original := {
		AliasPermission(Permission.read | Permission.write): 23
		unsafe { AliasPermission(0) }:                         29
	}
	decoded := json.decode[map[AliasPermission]int](json.encode(original))!
	assert decoded == original
}

enum SpellKey {
	shield = 1
	haste  = 7
}

type SpellKeyAlias = SpellKey

enum JsonAliasMapKey {
	alpha  @[json: 'beta']
	beta   @[json: 'gamma']
}

struct SpellEffect {
	duration int
}

struct SpellBook {
	effects map[SpellKey]SpellEffect
}

fn test_spell_enum_map_keys_round_trip() {
	spells := {
		SpellKey.shield: SpellEffect{ duration: 3 }
		SpellKey.haste:  SpellEffect{ duration: 5 }
	}
	assert json.decode[map[SpellKey]SpellEffect](json.encode(spells))! == spells
	book := SpellBook{ effects: spells }
	assert json.decode[SpellBook](json.encode(book))! == book
}

fn test_spell_enum_map_keys_unescape_and_keep_alias_type() {
	spells := json.decode[map[SpellKeyAlias]int]('{"sh\\u0069eld":3,"haste":5}')!
	assert spells[SpellKeyAlias(.shield)] == 3
	assert spells[SpellKeyAlias(.haste)] == 5
}

fn test_enum_map_key_json_alias_does_not_shadow_member_name() {
	original := {
		JsonAliasMapKey.alpha: 1
		JsonAliasMapKey.beta:  2
	}
	decoded := json.decode[map[JsonAliasMapKey]int](json.encode(original))!
	assert decoded.len == 2
	assert decoded[JsonAliasMapKey.alpha] == 1
	assert decoded[JsonAliasMapKey.beta] == 2
}

fn test_enum_map_keys_only_accept_member_names() {
	assert json.decode[JsonAliasMapKey]('"gamma"')! == .beta
	if _ := json.decode[map[JsonAliasMapKey]int]('{"gamma":2}') {
		assert false, 'JSON attributes should not rename enum map keys'
	} else {
		assert err.msg().contains('does not match any field in enum')
	}
}

fn test_unknown_enum_map_key_returns_error() {
	if _ := json.decode[map[SpellKey]int]('{"unknown":3}') {
		assert false, 'unknown enum map keys should fail'
	} else {
		assert err.msg().contains('does not match any field in enum')
	}
}
