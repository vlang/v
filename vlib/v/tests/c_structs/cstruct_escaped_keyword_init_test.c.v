#include "@VEXEROOT/vlib/v/tests/testdata/c_escaped_fields.h"

import os

@[typedef]
struct C.EscapedFieldRecord {
pub mut:
	type u32
}

type KeywordRecord = C.EscapedFieldRecord

struct EmbeddedKeywordRecord {
	C.EscapedFieldRecord
}

struct VEscapedKeywordPart {
mut:
	@type int
}

struct MixedEscapedKeywordRecord {
	VEscapedKeywordPart
	C.EscapedFieldRecord
}

fn test_escaped_keyword_matches_a_native_field_declared_without_escape() {
	mut value := C.EscapedFieldRecord{ @type: 42 }
	assert value.@type == 42
	value.@type = 7
	assert value.@type == 7
	alias := KeywordRecord{ @type: 9 }
	assert alias.@type == 9
}

fn test_escaped_keyword_initializes_promoted_native_field() {
	value := EmbeddedKeywordRecord{ @type: 42 }
	assert value.EscapedFieldRecord.@type == 42
}

fn test_exact_promoted_v_field_precedes_plain_c_keyword_field() {
	value := MixedEscapedKeywordRecord{ @type: 59 }
	assert value.VEscapedKeywordPart.@type == 59
	assert value.EscapedFieldRecord.@type == 0
}

fn test_sibling_c_embed_does_not_enable_plain_v_field_escape() {
	path := os.join_path(os.vtmp_dir(), 'v3_sibling_c_keyword_${os.getpid()}.c.v')
	defer { os.rm(path) or {} }
	os.write_file(path, '#include "@VEXEROOT/vlib/v/tests/testdata/c_escaped_fields.h"\n@[typedef]\nstruct C.EscapedFieldRecord {\npub mut:\n type u32\n}\nstruct VPart {\nmut:\n type string\n}\nstruct Wrapper {\n VPart\n C.EscapedFieldRecord\n}\nfn main() { _ := Wrapper{ @type: "text" } }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('expected `u32`, not `string`'), result.output
}

fn test_plain_keyword_field_in_v_struct_cannot_use_escape() {
	path := os.join_path(os.vtmp_dir(), 'v3_plain_v_struct_field_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct S { type int }\nfn main() { s := S{type: 1}; _ := s.@type }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('no field named `@type`'), result.output
}
