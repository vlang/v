module fastc

fn zygote_test_layout() ([]string, FastcUnitLayout) {
	helpers := 'typedef int word;\n#if defined(WIDE)\nstatic long twice(long value) {\n\treturn value * 2;\n}\n#else\nstatic int twice(int value) {\n\tif (value) {\n\t\treturn value * 2;\n\t}\n\treturn 0;\n}\n#endif\nstatic int counter = 3;\nstatic const char *label = name("x");\n'
	mut pieces := []string{}
	mut units := FastcUnitLayout{}
	fastc_append_split_head_piece(mut pieces, mut units.extern_indexes, mut units.extern_texts, mut
		units.define_texts, helpers)
	units.extern_indexes << pieces.len
	units.extern_texts << 'extern const u8 __vf_embed_kept[];\n'
	units.define_texts << 'const u8 __vf_embed_kept[] = {1};\n'
	pieces << 'const u8 __vf_embed_kept[] = {1};\n'
	// An embedded file that a payload object defines is declared in every unit.
	units.extern_indexes << pieces.len
	units.extern_texts << 'extern const u8 __vf_embed_moved[];\n'
	units.define_texts << 'extern const u8 __vf_embed_moved[];\n'
	pieces << 'const u8 __vf_embed_moved[] = {2};\n'
	pieces << 'int first(void);\nint second(void);\n'
	units.head_end = pieces.len
	units.solo_starts << pieces.len
	pieces << 'int runtime(void) { return 1; }\n'
	units.solo_starts << pieces.len
	pieces << 'void init(void) {'
	pieces << '\n\tcounter = 4;\n'
	pieces << '}\n'
	units.solo_end = pieces.len
	units.unit_starts << pieces.len
	pieces << 'int first(void) { return 1; }\n'
	units.unit_starts << pieces.len
	pieces << 'int second(void) { return 2; }\n'
	units.unit_starts << pieces.len
	return pieces, units
}

fn test_external_definitions_keep_only_what_the_declarations_leave_out() {
	pieces, units := zygote_test_layout()
	assert units.extern_texts[0].contains('extern int twice(int value);')
	assert units.extern_texts[0].contains('extern int counter;')
	definitions := fastc_external_definitions(pieces[0])
	// The conditional keeps one alternative of `twice`; the typedef and the static with a
	// call in its initializer already are in every unit.
	assert definitions == '#if defined(WIDE)\nlong twice(long value) {\n\treturn value * 2;\n}\n#else\nint twice(int value) {\n\tif (value) {\n\t\treturn value * 2;\n\t}\n\treturn 0;\n}\n#endif\nint counter = 3;\n'
	assert fastc_external_definitions('typedef int word;\n') == ''
	assert fastc_c_conditional_directive('#  ifndef X')
	assert fastc_c_conditional_directive('#elif Y')
	assert fastc_c_conditional_directive('#endif')
	assert !fastc_c_conditional_directive('#define if 1')
	assert !fastc_c_conditional_directive('#include <elf.h>')
}

fn test_zygote_definitions_skip_what_another_object_defines() {
	pieces, units := zygote_test_layout()
	definitions := fastc_zygote_definitions(pieces, &units)
	assert definitions.contains('int counter = 3;')
	assert definitions.contains('const u8 __vf_embed_kept[] = {1};')
	assert !definitions.contains('__vf_embed_moved')
	assert !definitions.contains('typedef')
}

fn test_zygote_plan_keeps_marked_definitions_whole() {
	pieces, units := zygote_test_layout()
	plan := fastc_zygote_plan('out', pieces, units, 8)
	// The text is far below one unit's worth, so it forms the least number of units.
	assert plan.paths == ['out.unit0.c', 'out.unit1.c']
	assert plan.unit_items.len == 3
	assert plan.unit_items[0] == 0
	// The definitions are one item, as small marked groups join; two body units follow.
	assert plan.solo_pieces == [units.head_end, units.solo_end]
	assert plan.unit_items[2] == 3
	assert plan.definitions.contains('int counter = 3;')
	// A program of one body unit does not split.
	mut single := units
	single.unit_starts = [units.unit_starts[0], units.unit_starts[2]]
	assert fastc_zygote_plan('out', pieces, single, 8).paths.len == 0
	assert fastc_zygote_plan('out', pieces, units, 1).paths.len == 0
}

fn test_zygote_plan_spreads_large_definition_groups() {
	mut pieces := ['int head;\n']
	mut units := FastcUnitLayout{
		head_end: 1
	}
	filler := '/* ${'x'.repeat(fastc_zygote_unit_bytes)} */\n'
	for _ in 0 .. 4 {
		units.solo_starts << pieces.len
		pieces << 'void group(void) {'
		pieces << filler
		pieces << '}\n'
	}
	units.solo_end = pieces.len
	for _ in 0 .. 4 {
		units.unit_starts << pieces.len
		pieces << filler
	}
	units.unit_starts << pieces.len
	plan := fastc_zygote_plan('out', pieces, units, 4)
	assert plan.paths.len == 4
	// Every definition group starts an item and stays in one piece range.
	assert plan.solo_pieces == [1, 4, 7, 10, 13]
	assert plan.unit_items == [0, 2, 4, 6, 8]
	// Without marks the definitions stay together.
	mut unmarked := units
	unmarked.solo_starts = []
	whole := fastc_zygote_plan('out', pieces, unmarked, 4)
	assert whole.solo_pieces == [1, 13]
	assert whole.unit_items.len == 5
	assert whole.unit_items[1] == 1
}

fn test_zygote_head_of_the_previous_build_is_matched_by_text() {
	pieces, units := zygote_test_layout()
	slots := fastc_zygote_extern_slots(&units)
	mut head := ''
	for k in 0 .. units.head_end {
		head += if slots[k] >= 0 { units.extern_texts[slots[k]] } else { pieces[k] }
	}
	// Every unit sees the declarations, never the definitions, of the split pieces.
	assert head.contains('extern int counter;')
	assert head.contains('extern const u8 __vf_embed_kept[];')
	assert !head.contains('int counter = 3;')
	assert fastc_zygote_head_matches(head, pieces, &units)
	assert !fastc_zygote_head_matches(head + ' ', pieces, &units)
	assert !fastc_zygote_head_matches(head[..head.len - 1], pieces, &units)
	assert !fastc_zygote_head_matches(head.replace('second', 'third!'), pieces, &units)
	assert !fastc_zygote_head_matches('', pieces, &units)
	// The kept file holds the options before the text; neither needs escaping.
	options := '-std=gnu11 "-I/a b"\n-DX'
	kept := fastc_parse_zygote_head(fastc_zygote_head_prefix(options) + head) or { panic('no head') }
	assert kept.options == options
	assert kept.text == head
	assert !kept.fed
	assert fastc_parse_zygote_head(head) == none
	assert fastc_parse_zygote_head(fastc_zygote_head_format + '99\nshort') == none
	assert fastc_parse_zygote_head(fastc_zygote_head_format + '-1\nshort') == none
	assert fastc_parse_zygote_head(fastc_zygote_head_format + '2x\nshort') == none
	empty := fastc_parse_zygote_head(fastc_zygote_head_prefix('')) or { panic('no head') }
	assert empty.options == '' && empty.text == ''
}

fn test_zygote_plan_names_its_objects_on_one_line() {
	pieces, units := zygote_test_layout()
	plan := fastc_zygote_plan('build dir/src', pieces, units, 8)
	assert plan.object_prefix == 'build dir/src.unit'
	assert plan.paths == ['build dir/src.unit0.c', 'build dir/src.unit1.c']
	assert fastc_zygote_plan('build\ndir/src', pieces, units, 8).paths.len == 0
	assert fastc_zygote_plan('build\\dir/src', pieces, units, 8).paths.len == 0
	assert fastc_zygote_plan('x'.repeat(1000), pieces, units, 8).paths.len == 0
}

fn test_zygote_rejects_options_that_hide_its_marker() {
	inactive := FastcZygote{}
	assert !fastc_zygote_accepts(&inactive, ['-std=gnu11'])
	active := FastcZygote{
		active: true
	}
	assert fastc_zygote_accepts(&active, ['-std=gnu11', '-Werror=implicit-function-declaration'])
	assert !fastc_zygote_accepts(&active, ['-std=gnu11', '-w'])
	assert !fastc_zygote_accepts(&active, ['-Werror'])
	assert fastc_zygote_marker.starts_with('#warning ')
}
