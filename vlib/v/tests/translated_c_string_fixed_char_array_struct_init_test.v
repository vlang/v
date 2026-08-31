@[translated]
module main

type SignedName = [9]i8
type SignedNameAlias = SignedName
type UnsignedName = [4]u8
type CharName = [4]char
type ByteAlias = u8
type UnsignedNamePointer = &UnsignedName

struct BoundaryFixedNames {
	exact_i8            [3]i8
	short_i8            [4]i8
	exact_u8            [3]u8
	short_u8            [4]u8
	exact_char          [3]char
	short_char          [4]char
	exact_element_alias [3]ByteAlias
	utf8_exact          [2]u8
	utf8_exact_i8       [2]i8
	utf8_exact_char     [2]char
	utf8_element_alias  [2]ByteAlias
	escaped_char        [1]char
	nul_char            [1]u8
	embedded_nul        [3]char
	escaped_backslash   [1]u8
	octal_maximal       [2]u8
	hex_then_plain      [2]u8
}

struct FixedNames {
	short_name   SignedNameAlias
	exact_name   UnsignedName
	char_name    CharName
	pointer_name &char
}

struct GenericFixedName[T] {
	name [4]T
}

struct DecoratedFixedNamePointers {
	direct_pointer   &UnsignedName
	optional_pointer ?&UnsignedName
	pointer_alias    UnsignedNamePointer
	signed_pointer   &SignedName
}

const global_fixed_names = FixedNames{c'abc', c'WXYZ', c'char', c'pointer'}

fn test_translated_c_string_initializes_global_fixed_char_array_fields() {
	assert global_fixed_names.short_name[0] == i8(`a`)
	assert global_fixed_names.short_name[3] == 0
	assert global_fixed_names.exact_name[3] == u8(`Z`)
	assert global_fixed_names.char_name[3] == char(`r`)
	assert global_fixed_names.pointer_name[0] == char(`p`)
}

fn test_translated_c_string_initializes_local_fixed_char_array_fields() {
	local := FixedNames{
		short_name:   c'local'
		exact_name:   c'four'
		char_name:    c'char'
		pointer_name: c'pointer'
	}
	assert local.short_name[4] == i8(`l`)
	assert local.short_name[5] == 0
	assert local.exact_name[3] == u8(`r`)
	assert local.char_name[0] == char(`c`)
	assert local.pointer_name[1] == char(`o`)
}

fn test_translated_c_string_initializes_heap_fixed_char_array_fields() {
	heap := &FixedNames{
		short_name:   c'heap'
		exact_name:   c'full'
		char_name:    c'char'
		pointer_name: c'pointer'
	}
	assert heap.short_name[3] == i8(`p`)
	assert heap.short_name[4] == 0
	assert heap.exact_name[3] == u8(`l`)
	assert heap.char_name[2] == char(`a`)
	assert heap.pointer_name[2] == char(`i`)
}

fn test_translated_c_string_fixed_char_array_boundaries() {
	value := BoundaryFixedNames{
		exact_i8:            c'abc'
		short_i8:            c'abc'
		exact_u8:            c'abc'
		short_u8:            c'abc'
		exact_char:          c'abc'
		short_char:          c'abc'
		exact_element_alias: c'abc'
		utf8_exact:          c'é'
		utf8_exact_i8:       c'é'
		utf8_exact_char:     c'é'
		utf8_element_alias:  c'é'
		escaped_char:        c'\n'
		nul_char:            c'\0'
		embedded_nul:        c'a\0b'
		escaped_backslash:   c'\\'
		octal_maximal:       c'\1412'
		hex_then_plain:      c'\x41z'
	}
	assert value.exact_i8[2] == i8(`c`)
	assert value.short_i8[3] == 0
	assert value.exact_u8[2] == u8(`c`)
	assert value.short_u8[3] == 0
	assert value.exact_char[2] == char(`c`)
	assert value.short_char[3] == char(0)
	assert value.exact_element_alias[2] == ByteAlias(`c`)
	assert value.utf8_exact == [u8(0xc3), 0xa9]!
	assert value.utf8_exact_i8 == [i8(-61), -87]!
	assert u8(value.utf8_exact_char[0]) == 0xc3
	assert u8(value.utf8_exact_char[1]) == 0xa9
	assert value.utf8_element_alias == [ByteAlias(0xc3), 0xa9]!
	assert value.escaped_char[0] == char(10)
	assert value.nul_char[0] == 0
	assert value.embedded_nul[0] == char(`a`)
	assert value.embedded_nul[1] == char(0)
	assert value.embedded_nul[2] == char(`b`)
	assert value.escaped_backslash[0] == 92
	assert value.octal_maximal == [u8(`a`), `2`]!
	assert value.hex_then_plain == [u8(`A`), `z`]!
}

fn test_translated_c_string_initializes_concrete_generic_fixed_char_array_field() {
	value := GenericFixedName[u8]{
		name: c'abcd'
	}
	assert value.name[0] == u8(`a`)
	assert value.name[3] == u8(`d`)
}

fn test_translated_c_string_initializes_fixed_char_array_pointer_fields() {
	value := DecoratedFixedNamePointers{
		direct_pointer:   c'abcd'
		optional_pointer: c'efgh'
		pointer_alias:    c'ijkl'
		signed_pointer:   c'signedptr'
	}
	assert value.direct_pointer[0] == u8(`a`)
	assert value.direct_pointer[3] == u8(`d`)
	assert (*value.pointer_alias)[0] == u8(`i`)
	assert (*value.pointer_alias)[3] == u8(`l`)
	assert value.signed_pointer[0] == i8(`s`)
	assert value.signed_pointer[8] == i8(`r`)
}
