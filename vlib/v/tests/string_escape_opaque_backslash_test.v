import strings
import v.reflection
import json
import db.sqlite

// A \xXX/\uXXXX/\UXXXXXXXX escape that decodes to a backslash byte (0x5C) must never be
// reinterpreted as the start of a different escape sequence, even when the byte(s) that follow
// it in the source spell out a recognized escape letter (e.g. `n`, `t`).

fn test_hex_escape_decoding_to_backslash_followed_by_letter() {
	s := 'A\x5cnB'
	assert s.len == 4
	assert s.bytes() == [u8(65), 92, 110, 66]
}

fn test_u32_escape_decoding_to_backslash_followed_by_letter() {
	s := 'A\U0000005crB'
	assert s.len == 4
	assert s.bytes() == [u8(65), 92, 114, 66]
}

fn test_trailing_decoded_backslash() {
	s := 'A\x5c'
	assert s.len == 2
	assert s.bytes() == [u8(65), 92]
}

fn test_two_decoded_backslashes_back_to_back() {
	s := 'A\x5c\x5cB'
	assert s.len == 4
	assert s.bytes() == [u8(65), 92, 92, 66]
}

fn test_interpolated_string_with_decoded_backslash() {
	x := 42
	s := 'A\x5cn${x}B'
	assert s.len == 6
	assert s.bytes() == [u8(65), 92, 110, 52, 50, 66]
}

// regression test for a gap found by adversarial review of the fix above: the transformer's
// simplify_nested_interpolation_in_sb splits an interpolated string into per-segment literals
// for @[expand_simple_interpolation] calls (like strings.Builder.write_string/writeln), and
// must carry the same opaque-byte protection through to each split-out segment.
fn test_string_builder_write_string_with_decoded_backslash_in_interpolation() {
	x := 1
	mut sb := strings.new_builder(16)
	sb.write_string('A\x5cnB${x}')
	s := sb.str()
	assert s.len == 5
	assert s.bytes() == [u8(65), 92, 110, 66, 49]
}

fn test_compile_time_string_concat_folding_with_decoded_backslash() {
	s := 'A\x5c' + 'nB'
	assert s.len == 4
	assert s.bytes() == [u8(65), 92, 110, 66]
}

fn test_decoded_backslash_does_not_break_string_equality_and_match() {
	s := 'A\x5cnB'
	assert s == 'A\x5cnB'
	mut matched := false
	match s {
		'A\x5cnB' { matched = true }
		else {}
	}
	assert matched
}

fn test_ordinary_escapes_still_work() {
	assert 'A\nB'.bytes() == [u8(65), 10, 66]
	assert 'A\\B'.bytes() == [u8(65), 92, 66]
	assert 'A\x22B'.bytes() == [u8(65), 34, 66]
}

// regression test for a second gap found and fixed after adversarial review: ast.Attr's `name`
// and `arg` fields are populated from the same scanner-tokenized string literals as ordinary
// strings (e.g. `@[mytag: 'A\x5cnB']` or the whole-attribute-is-a-string form `@['A\x5cnB']`),
// but previously had no opaque_pos carrier, so the hazard reproduced there too.

@[mytag: 'A\x5cnB']
fn attr_tagged_fn() {}

fn test_attr_arg_with_decoded_backslash_via_reflection() {
	mut found := false
	for f in reflection.get_funcs() {
		if f.name.ends_with('attr_tagged_fn') {
			for a in f.attrs {
				if a.name == 'mytag' {
					found = true
					assert a.arg.len == 4
					assert a.arg.bytes() == [u8(65), 92, 110, 66]
				}
			}
		}
	}
	assert found
}

struct AttrHazardStruct {
	a int @[mytag: 'A\x5cnB']
	b int @['C\x5cnD']
}

fn test_attr_name_and_combined_string_with_decoded_backslash_via_comptime() {
	mut seen := 0
	$for field in AttrHazardStruct.fields {
		for a in field.attrs {
			if field.name == 'a' {
				// combined "name: 'arg'" display string built by cgen_attrs()
				assert a.bytes() == [u8(109), 121, 116, 97, 103, 58, 32, 39, 65, 92, 110, 66, 39]
				seen++
			}
			if field.name == 'b' {
				// whole-attribute-is-a-string form: the decoded backslash sits directly in `name`
				assert a.bytes() == [u8(67), 92, 110, 68]
				seen++
			}
		}
	}
	assert seen == 2
}

// regression test for a third gap found by a second adversarial review round (after the fix
// above was fast-forwarded onto a newer master): comptime_for's `.attributes` branch (reached
// via `$for attr in T.attributes`, distinct from the `.fields` path above) built its VAttribute's
// `.name` field by interpolating attr.name raw, bypassing util.smart_quote()/name_opaque_pos
// entirely, even though the sibling `.arg` field two lines below it was already correctly fixed.
@['E\x5cnF']
struct AttrWholeStringStruct {}

fn test_attr_name_with_decoded_backslash_via_type_attributes() {
	mut seen := false
	$for attr in AttrWholeStringStruct.attributes {
		assert attr.name.len == 4
		assert attr.name.bytes() == [u8(69), 92, 110, 70]
		seen = true
	}
	assert seen
}

// regression test for a fourth gap found in the same review round: `$for method in T.methods`
// exposes a method's structured attributes via `.attributes` ([]VAttribute, built by
// cgen_vattrs()), a separate code path from both `.attrs` ([]string, via cgen_attrs(), already
// covered above) and v.reflection's function listing (via reflection.v's gen_attrs_array(),
// already covered above) - none of those three cover this fourth path.
struct MethodAttrHazardStruct {}

@[mytag: 'A\x5cnB']
fn (m MethodAttrHazardStruct) hazard_method() {}

fn test_method_attr_arg_with_decoded_backslash_via_comptime_methods() {
	mut found := false
	$for method in MethodAttrHazardStruct.methods {
		for a in method.attributes {
			if a.name == 'mytag' {
				found = true
				assert a.arg.len == 4
				assert a.arg.bytes() == [u8(65), 92, 110, 66]
			}
		}
	}
	assert found
}

// regression test for a fifth gap found in the same review round: an enum value's `@[json: ...]`
// attribute arg is spliced directly into generated C by vlib/v/gen/c/json.v's gen_enum_to_str /
// gen_str_to_enum, bypassing util.smart_quote()/arg_opaque_pos - a separate code path from every
// ast.Attr consumer covered above, in a file the original fix never touched.
enum JsonAttrHazardEnum {
	red
	blue @[json: 'A\x5cnB']
}

struct JsonAttrHazardWrap {
	c JsonAttrHazardEnum
}

fn test_json_enum_attr_arg_with_decoded_backslash() {
	encoded := json.encode(JsonAttrHazardWrap{ c: .blue })
	// the true attr value is 4 bytes (A, 0x5C, 'n', B); a spec-compliant JSON encoding of that
	// must double the backslash - a single backslash means the byte was corrupted before encode.
	assert encoded.bytes() == [u8(123), 34, 99, 34, 58, 34, 65, 92, 92, 110, 66, 34, 125]

	decoded := json.decode(JsonAttrHazardWrap, '{"c":"A\\\\nB"}') or { panic(err) }
	assert decoded.c == .blue
}

// regression test for a sixth gap found in the same review round: get_table_name_by_struct_type()
// (vlib/v/gen/c/orm.v) reads the ORM table name from a struct's @[table: ...] attribute arg but
// was passing a hardcoded empty opaque_pos to smart_quote instead of attr.arg_opaque_pos - the
// only smart_quote(attr.arg, ...) call site in orm.v that was missed during the original fix.
@[table: 'T\x5cnU']
struct OrmTableHazardItem {
	id int @[primary; sql: serial]
}

fn test_orm_table_name_with_decoded_backslash() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table OrmTableHazardItem
	} or { panic(err) }
	rows := db.exec('select name from sqlite_master where type = \'table\'') or { panic(err) }
	mut found := false
	for row in rows {
		if row.vals.len > 0 {
			found = true
			assert row.vals[0].bytes() == [u8(84), 92, 110, 85]
		}
	}
	assert found
}

// regression coverage for a gap identified but not itself a bug: parse_attr_call() (call-style
// attributes like @[foo(a, b: 'c')]) has three separate ast.Attr{} construction sites (the base
// attribute, a positional argument, and a named argument), none of which were exercised by any
// test above - all three use the colon/whole-string forms instead of call-style syntax.
@[mytag('A\x5cnB', 'C\x5cnD', extra: 'E\x5cnF')]
fn attr_call_style_hazard_fn() {}

fn test_attr_call_style_all_three_sites_with_decoded_backslash() {
	mut seen := map[string]bool{}
	for f in reflection.get_funcs() {
		if f.name.ends_with('attr_call_style_hazard_fn') {
			for a in f.attrs {
				match a.name {
					'mytag' {
						// base attribute: the first call argument
						assert a.arg.bytes() == [u8(65), 92, 110, 66]
					}
					'mytag_1' {
						// positional argument construction site
						assert a.arg.bytes() == [u8(67), 92, 110, 68]
					}
					'mytag_extra' {
						// named argument construction site
						assert a.arg.bytes() == [u8(69), 92, 110, 70]
					}
					else {}
				}
				seen[a.name] = true
			}
		}
	}
	assert seen.len == 3
}

// regression coverage for a gap identified but not itself a bug: every hazard-carrying attribute
// test above uses single-quote syntax; attribute values can also use double quotes.
@[mytag: "A\x5cnB"]
fn attr_double_quoted_hazard_fn() {}

fn test_attr_double_quoted_arg_with_decoded_backslash() {
	mut found := false
	for f in reflection.get_funcs() {
		if f.name.ends_with('attr_double_quoted_hazard_fn') {
			for a in f.attrs {
				if a.name == 'mytag' {
					found = true
					assert a.arg.bytes() == [u8(65), 92, 110, 66]
				}
			}
		}
	}
	assert found
}
