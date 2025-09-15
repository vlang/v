enum NonSpecificEnum {
	a = 1
	b
}

enum Enum8 as u8 {
	a = 1
	b
}
enum Enum16 as u16 {
	a = 1
	b
}
enum Enum32 as u32 {
	a = 1
	b
}
enum Enum64 as u64 {
	a = 1
	b
}

// TODO: make a generic version, and call it several times

fn test_check_map_with_enum_key() {
	dump(sizeof(NonSpecificEnum))
	mut m := map[NonSpecificEnum]string{}
	m[.a] = 'a'
	m[.b] = 'b'
	dump(m)
	dump(m[.a])
	dump(m[.b])
	assert m[.a] == 'a'
	assert m[.b] == 'b'
}

fn test_check_map_with_enum_key_8() {
	dump(sizeof(Enum8))
	mut m := map[Enum8]string{}
	m[.a] = 'a'
	m[.b] = 'b'
	dump(m)
	dump(m[.a])
	dump(m[.b])
	assert m[.a] == 'a'
	assert m[.b] == 'b'
}

fn test_check_map_with_enum_key_16() {
	dump(sizeof(Enum16))
	mut m := map[Enum16]string{}
	m[.a] = 'a'
	m[.b] = 'b'
	dump(m)
	dump(m[.a])
	dump(m[.b])
	assert m[.a] == 'a'
	assert m[.b] == 'b'
}

fn test_check_map_with_enum_key_32() {
	dump(sizeof(Enum32))
	mut m := map[Enum32]string{}
	m[.a] = 'a'
	m[.b] = 'b'
	dump(m)
	dump(m[.a])
	dump(m[.b])
	assert m[.a] == 'a'
	assert m[.b] == 'b'
}

fn test_check_map_with_enum_key_64() {
	dump(sizeof(Enum64))
	mut m := map[Enum64]string{}
	m[.a] = 'a'
	m[.b] = 'b'
	dump(m)
	dump(m[.a])
	dump(m[.b])
	assert m[.a] == 'a'
	assert m[.b] == 'b'
}
