#include "@VMODROOT/vlib/v/tests/testdata/c_escaped_fields.h"

@[translated]
module main

@[typedef]
struct C.EscapedFieldPlain {
pub mut:
	type u32
}

struct EscapedPlainWrapper {
	C.EscapedFieldPlain
}

struct EscapedPlainShadowWrapper {
	C.EscapedFieldPlain
mut:
	@type int
}

fn test_promoted_plain_c_keyword_field() {
	mut wrapper := EscapedPlainWrapper{}
	wrapper.@type = 7
	assert wrapper.@type == 7
}

fn test_direct_escaped_field_shadows_plain_c_keyword_field() {
	mut wrapper := EscapedPlainShadowWrapper{
		@type: 59
	}
	assert wrapper.@type == 59
	wrapper.@type = 61
	assert wrapper.@type == 61
}
