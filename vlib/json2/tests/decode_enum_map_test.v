import json2

enum MapKey {
	first
	second = 7
}

type AliasMapKey = MapKey

@[flag]
enum Permission {
	read
	write
}

type AliasPermission = Permission

struct EnumMapHolder {
	entries map[MapKey]int
}

fn test_decode_enum_map_keys() {
	decoded := json2.decode[map[MapKey]int]('{"first":11,"second":27}')!
	assert decoded[.first] == 11
	assert decoded[.second] == 27
}

fn test_enum_map_keys_round_trip_in_struct() {
	original := EnumMapHolder{
		entries: {
			MapKey.second: 42
		}
	}
	decoded := json2.decode[EnumMapHolder](json2.encode(original))!
	assert decoded == original
}

fn test_decode_invalid_enum_map_key() {
	json2.decode[map[MapKey]int]('{"missing":1}') or {
		assert err.msg().contains('missing')
		return
	}
	assert false
}

fn test_decode_alias_enum_map_key() {
	decoded := json2.decode[map[AliasMapKey]int]('{"second":42}')!
	assert decoded[AliasMapKey(MapKey.second)] == 42
}

fn test_flag_enum_map_keys_round_trip() {
	for permission in [Permission.read, Permission.write, Permission.read | Permission.write,
		Permission.read & Permission.write] {
		original := {
			permission: 7
		}
		encoded := json2.encode(original)
		decoded := json2.decode[map[Permission]int](encoded)!
		assert decoded == original, encoded
	}
}

fn test_alias_flag_enum_map_keys_round_trip() {
	permission := AliasPermission(Permission.read | Permission.write)
	original := {
		permission: 7
	}
	decoded := json2.decode[map[AliasPermission]int](json2.encode(original))!
	assert decoded == original
}

fn test_flag_enum_map_keys_reject_invalid_values() {
	for key in ['Permission{.missing}', 'Permission{.read | .missing}', 'Other{.read}',
		'Permission{.read', 'Permission{read}'] {
		if _ := json2.decode[map[Permission]int]('{"${key}":7}') {
			assert false, 'invalid flag key was accepted: ${key}'
		}
	}
}
