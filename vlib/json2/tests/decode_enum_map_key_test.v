import json2

enum SpellKey {
	shield = 1
	haste  = 7
}

type SpellKeyAlias = SpellKey

enum MapKey {
	alpha  @[json: 'beta']
	beta   @[json: 'gamma']
}

struct SpellEffect {
	duration int
}

struct SpellBook {
	effects map[SpellKey]SpellEffect
}

fn test_enum_map_keys_round_trip() {
	spells := {
		SpellKey.shield: SpellEffect{ duration: 3 }
		SpellKey.haste:  SpellEffect{ duration: 5 }
	}
	assert json2.decode[map[SpellKey]SpellEffect](json2.encode(spells))! == spells
	book := SpellBook{ effects: spells }
	assert json2.decode[SpellBook](json2.encode(book))! == book
}

fn test_enum_map_keys_unescape_and_keep_alias_type() {
	spells := json2.decode[map[SpellKeyAlias]int]('{"sh\\u0069eld":3,"haste":5}')!
	assert spells[SpellKeyAlias(.shield)] == 3
	assert spells[SpellKeyAlias(.haste)] == 5
}

fn test_enum_map_key_json_alias_does_not_shadow_member_name() {
	original := {
		MapKey.alpha: 1
		MapKey.beta:  2
	}
	decoded := json2.decode[map[MapKey]int](json2.encode(original))!
	assert decoded.len == 2
	assert decoded[MapKey.alpha] == 1
	assert decoded[MapKey.beta] == 2
}

fn test_enum_map_keys_only_accept_member_names() {
	assert json2.decode[MapKey]('"gamma"')! == .beta
	if _ := json2.decode[map[MapKey]int]('{"gamma":2}') {
		assert false, 'JSON attributes should not rename enum map keys'
	} else {
		assert err.msg().contains('does not match any field in enum')
	}
}

fn test_unknown_enum_map_key_returns_error() {
	if _ := json2.decode[map[SpellKey]int]('{"unknown":3}') {
		assert false, 'unknown enum map keys should fail'
	} else {
		assert err.msg().contains('does not match any field in enum')
	}
}
