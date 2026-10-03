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

enum PlainKeywordCount {
	first
	struct = 4
}

struct KeywordSized {
	data [int(PlainKeywordCount.@struct)]u8
}

enum DistinctKeywordMembers {
	none  = 2
	@none = 4
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
	assert KeywordSized{}.data.len == 4
}

fn test_escaped_and_plain_enum_members_have_distinct_match_coverage() {
	path := os.join_path(os.vtmp_dir(), 'v3_distinct_enum_coverage_${os.getpid()}.v')
	os.write_file(path, 'enum Choice { none @none }\nfn f(e Choice) int { match e { .@none { return 1 } } }\nfn main() {}\n')!
	defer { os.rm(path) or {} }
	result := os.exec([@VEXE, '-new-compiler', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('missing return'), result.output
}

fn test_escaped_enum_members_in_constant_array_sizes() {
	plain := [int(PlainKeywordCount.@struct)]u8{}
	unescaped := [int(DistinctKeywordMembers.none)]u8{}
	escaped := [int(DistinctKeywordMembers.@none)]u8{}
	assert plain.len == 4
	assert unescaped.len == 2
	assert escaped.len == 4
}

fn test_escaped_enum_members_keep_distinct_match_coverage() {
	path := os.join_path(os.vtmp_dir(), 'v3_distinct_enum_escape_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for condition in ['.@none', 'Kind.@none'] {
		os.write_file(path, 'enum Kind { none = 2 @none = 4 }\nfn score(value Kind) int { match value { ${condition} { return 4 } } }\nfn main() { println(score(.none)) }\n')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('missing return'), result.output
	}
}

fn test_non_keyword_enum_member_cannot_be_escaped() {
	path := os.join_path(os.vtmp_dir(), 'v3_invalid_enum_escape_${os.getpid()}.v')
	os.write_file(path, 'enum Kind { left struct }\nfn main() { _ := Kind.@left }\n')!
	defer { os.rm(path) or {} }
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('only escape keyword enum members'), result.output
}

fn test_native_backend_preserves_escaped_enum_declaration() {
	$if arm64 {
		path := os.join_path(os.vtmp_dir(), 'v3_native_escaped_enum_${os.getpid()}.v')
		defer { os.rm(path) or {} }
		os.write_file(path, 'enum Kind { @none = -10 struct }\nenum Distinct { none = 2 @none = 4 }\nfn main() { assert int(Kind.@none) == -10; assert int(Kind.none) == -10; assert int(Kind.@struct) == -9; value := Kind.none; assert value == .none; assert int(Distinct.none) == 2; assert int(Distinct.@none) == 4 }\n')!
		result := os.exec([@VEXE, '-b', 'arm64', '-gc', 'none', 'run', path])
		assert result.exit_code == 0, result.output
	}
}

enum EscapedFirstDefault {
	@none = -10
	none  = 5
}

struct EscapedDefaultHolder {
	value EscapedFirstDefault
}

fn test_enum_default_keeps_the_first_declaration_identity() {
	assert int(EscapedDefaultHolder{}.value) == -10
	assert int(EscapedDefaultHolder{ value: .none }.value) == 5
	assert int(EscapedFirstDefault.@none) == -10
	assert int(EscapedFirstDefault.none) == 5
}

fn alignment_score_plain_keyword(value Alignment) int {
	match value {
		.none { return 0 }
		.left { return 1 }
		.@type { return 2 }
	}
}

fn test_plain_keyword_reference_resolves_an_escaped_only_declaration() {
	assert alignment_score_plain_keyword(.@none) == 0
	assert alignment_score_plain_keyword(.@type) == 2
}

enum KeywordInitializer {
	struct        = 4
	from_plain    = int(KeywordInitializer.@struct) + 6
	@none         = 11
	from_escaped  = int(KeywordInitializer.none) + 2
	type          = 17
	@type         = 23
	plain_exact   = int(KeywordInitializer.type) + 3
	escaped_exact = int(KeywordInitializer.@type) + 3
}

fn test_enum_initializers_keep_keyword_reference_identity_in_constant_sizes() {
	from_plain := [int(KeywordInitializer.from_plain)]u8{}
	from_escaped := [int(KeywordInitializer.from_escaped)]u8{}
	plain_exact := [int(KeywordInitializer.plain_exact)]u8{}
	escaped_exact := [int(KeywordInitializer.escaped_exact)]u8{}
	assert from_plain.len == 10
	assert from_escaped.len == 13
	assert plain_exact.len == 20
	assert escaped_exact.len == 26
	assert from_plain.len == int(KeywordInitializer.from_plain)
	assert from_escaped.len == int(KeywordInitializer.from_escaped)
}

fn test_native_backend_evaluates_escaped_enum_initializer_references() {
	$if arm64 {
		path := os.join_path(os.vtmp_dir(), 'v3_native_escaped_enum_initializer_${os.getpid()}.v')
		defer { os.rm(path) or {} }
		os.write_file(path, 'enum Kind { struct = 4 next = int(Kind.@struct) + 6 @none = 11 reverse = int(Kind.none) + 2 type = 17 @type = 23 plain_exact = int(Kind.type) + 3 escaped_exact = int(Kind.@type) + 3 }\nfn main() { assert int(Kind.next) == 10; assert int(Kind.reverse) == 13; assert int(Kind.plain_exact) == 20; assert int(Kind.escaped_exact) == 26; value := Kind.next; assert value.str() == "next" }\n')!
		result := os.exec([@VEXE, '-b', 'arm64', '-gc', 'none', 'run', path])
		assert result.exit_code == 0, result.output
	}
}
