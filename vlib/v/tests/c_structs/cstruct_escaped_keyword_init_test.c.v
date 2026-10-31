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

struct PlainKeywordPart {
	type string
}

struct MixedKeywordRecord {
	PlainKeywordPart
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
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('expected `u32`, not `string`'), result.output
}

fn test_plain_keyword_field_in_v_struct_cannot_use_escape() {
	path := os.join_path(os.vtmp_dir(), 'v3_plain_v_struct_field_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct S { type int }\nfn main() { s := S{type: 1}; _ := s.@type }\n')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('no field named `@type`'), result.output
}

fn test_exact_promoted_escaped_v_field_precedes_c_fallback() {
	mut value := MixedEscapedKeywordRecord{ @type: 59 }
	assert value.@type == 59
	assert value.VEscapedKeywordPart.@type == 59
	assert value.EscapedFieldRecord.@type == 0
	value.@type = 61
	assert value.VEscapedKeywordPart.@type == 61
	assert value.EscapedFieldRecord.@type == 0
}

fn test_escaped_c_fallback_uses_the_c_owner_with_a_plain_v_sibling() {
	mut value := MixedKeywordRecord{ @type: u32(67) }
	assert value.EscapedFieldRecord.@type == 67
	assert value.@type == 67
	assert value.PlainKeywordPart.type == ''
	value.@type = 71
	assert value.EscapedFieldRecord.@type == 71
	assert value.PlainKeywordPart.type == ''
}

fn test_escaped_c_fallback_rejects_the_plain_v_sibling_type() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_field_sibling_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct C.Record { type u32 }\nstruct PlainPart { type string }\nstruct Wrapper { PlainPart C.Record }\nfn main() { _ := Wrapper{ @type: "text" } }\n')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('u32'), result.output
	assert result.output.contains('string'), result.output
}
