#include "@VEXEROOT/vlib/v/tests/testdata/c_escaped_fields.h"

@[typedef]
struct C.EscapedFieldRecord {
pub mut:
	type u32
}

type KeywordRecord = C.EscapedFieldRecord

fn test_escaped_keyword_matches_a_native_field_declared_without_escape() {
	mut value := C.EscapedFieldRecord{ @type: 42 }
	assert value.@type == 42
	value.@type = 7
	assert value.@type == 7
	alias := KeywordRecord{ @type: 9 }
	assert alias.@type == 9
}
