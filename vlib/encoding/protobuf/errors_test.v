module protobuf

fn test_error_messages_name_the_module_and_position() {
	// The byte offset is the only way to locate a problem inside a nested
	// payload, so every message has to carry it.
	eof := unexpected_eof_at(42, 4, 1)
	assert eof.msg() == 'protobuf: unexpected end of input at pos 42: need 4 bytes, have 1'

	bad := malformed_at(7, 'tag is zero')
	assert bad.msg() == 'protobuf: malformed at pos 7: tag is zero'

	wt := unknown_wire_type(3, 6)
	assert wt.msg() == 'protobuf: unknown wire type 6 at pos 3'

	depth := max_depth_exceeded(11, 100)
	assert depth.msg() == 'protobuf: message nesting deeper than 100 at pos 11'

	length := max_length_exceeded(5, 1 << 40, 1024)
	assert length.msg().contains('exceeds the 1024 byte limit')

	utf8 := invalid_utf8_at(9)
	assert utf8.msg() == 'protobuf: string field at pos 9 is not valid UTF-8'

	unknown := unknown_field_at(13, 77)
	assert unknown.msg() == 'protobuf: unknown field number 77 at pos 13'

	group := group_unsupported_at(17, 5)
	assert group.msg() == 'protobuf: group field 5 at pos 17 is not supported'

	mismatch := wire_type_mismatch(2, .varint, .length_delimited)
	assert mismatch.msg() ==
		'protobuf: field 2 cannot be read as wire type length_delimited, expected varint'

	missing := MissingFieldError{ name: 'query' }
	assert missing.msg() == 'protobuf: required field `query` is missing'
}

fn test_errors_are_pattern_matchable() {
	// The point of typed errors: a caller can branch on the kind without
	// matching on message text.
	handlers := fn (e IError) string {
		if e is UnexpectedEofError {
			return 'truncated'
		} else if e is WireTypeMismatchError {
			return 'mismatch'
		}
		return 'other'
	}
	assert handlers(unexpected_eof_at(0, 2, 0)) == 'truncated'
	assert handlers(wire_type_mismatch(1, .varint, .fixed32)) == 'mismatch'
	assert handlers(malformed_at(0, 'x')) == 'other'
}
