import os

enum Alignment {
	@none = -10
	left
	@type
}

enum PlainKeywordMember {
	left
	struct
}

struct Layout {
	alignment Alignment = .@none
}

fn alignment_name(value Alignment) string {
	return match value {
		.@none { 'unset' }
		.left { 'left' }
		.@type { 'type' }
	}
}

fn alignment_score(value Alignment) int {
	match value {
		.@none { return 0 }
		.left { return 1 }
		.@type { return 2 }
	}
}

fn test_escaped_enum_members_keep_their_type_and_value() {
	assert Layout{}.alignment == .@none
	assert int(Alignment.@none) == -10
	mut value := Alignment.left
	value = .@type
	assert value == Alignment.@type
	assert alignment_name(value) == 'type'
	value = .@none
	assert alignment_name(value) == 'unset'
	assert [Alignment.@none, .left, .@type].map(alignment_name(it)) == ['unset', 'left', 'type']
	assert alignment_score(.@none) == 0
	assert alignment_score(.@type) == 2
}

fn test_escaped_reference_to_plain_keyword_member() {
	assert int(PlainKeywordMember.@struct) == 1
	value := PlainKeywordMember.@struct
	assert value == .@struct
}

fn test_non_keyword_enum_member_cannot_be_escaped() {
	path := os.join_path(os.vtmp_dir(), 'v3_invalid_enum_escape_${os.getpid()}.v')
	os.write_file(path, 'enum Kind { left struct }\nfn main() { _ := Kind.@left }\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('only escape keyword enum members'), result.output
}
