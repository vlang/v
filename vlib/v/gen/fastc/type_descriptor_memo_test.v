module fastc

import v.flat
import v.pref
import v.scanner

@[unsafe]
fn fastc_descriptor_memo_test_parser(names flat.TypeTextBindings, aliases flat.TypeTextBindings, finalized bool) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:                 prefs
		s:                     scanner.new_scanner(prefs, .normal)
		selfhost:              true
		declared_type_c_names: names
		alias_base_types:      aliases
		type_descriptor_memo:  if finalized { &FastcTypeDescriptorMemo{} } else { unsafe { nil } }
	}
}

fn test_fastc_final_descriptor_memos_preserve_pointers_aliases_and_cycles() {
	mut names := flat.TypeTextBindings{}
	names.set('memo__Record', 'memo.Record')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('MemoCount', 'int')
	aliases.set('MemoNestedCount', 'MemoCount')
	aliases.set('MemoPointer', 'MemoCount*')
	aliases.set('MemoCycleA', 'MemoCycleB')
	aliases.set('MemoCycleB', 'MemoCycleA')
	aliases.set('MemoSelf', 'MemoSelf')
	g := fastc_descriptor_memo_test_parser(names, aliases, true)
	cases := {
		'int':              'int'
		'memo__Record':     'memo__Record'
		'MemoUnknown':      'MemoUnknown'
		'MemoCount':        'int'
		'MemoCount**':      'int**'
		'MemoNestedCount*': 'int*'
		'MemoPointer*':     'int**'
		'MemoCycleA':       'MemoCycleA'
		'MemoCycleB*':      'MemoCycleB*'
		'MemoSelf':         'MemoSelf'
	}
	for input, expected in cases {
		id := flat.source_type_id(input)
		expected_id := flat.source_type_id(expected)
		for _ in 0 .. 3 {
			assert g.underlying_alias_type(id) == expected_id, input
			assert g.underlying_alias_type(id) == fastc_underlying_alias_type(id, aliases), input
			assert g.semantic_type_key(id) == g.semantic_type_key_uncached(id), input
		}
		assert g.type_descriptor_memo.aliases[int(id & 63)].key == id
		assert g.type_descriptor_memo.semantic_keys[int(id & 63)].key == id
	}
	for input in ['memo__Record', 'memo__Record*', 'memo__Record***'] {
		id := flat.source_type_id(input)
		assert g.semantic_type_key(id) == flat.source_type_id('memo.Record')
	}
	assert g.semantic_type_key(flat.source_type_id('int**')) == flat.source_type_int
}

fn test_fastc_arbitrary_parser_descriptor_lookups_remain_uncached() {
	mut names := flat.TypeTextBindings{}
	names.set('memo__Mutable', 'memo.Before')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('MemoMutableAlias', 'int')
	// These bindings deliberately retain the same backing maps as the Parser.
	// Only the final file-generator constructor promises immutable descriptors.
	g := fastc_descriptor_memo_test_parser(names, aliases, false)
	typ := flat.source_type_id('memo__Mutable*')
	alias := flat.source_type_id('MemoMutableAlias*')
	assert g.semantic_type_key(typ) == flat.source_type_id('memo.Before')
	assert g.underlying_alias_type(alias) == flat.source_type_id('int*')
	names.set('memo__Mutable', 'memo.After')
	aliases.set('MemoMutableAlias', 'i64')
	assert g.semantic_type_key(typ) == flat.source_type_id('memo.After')
	assert g.underlying_alias_type(alias) == flat.source_type_id('i64*')
	assert isnil(g.type_descriptor_memo)
}

fn test_fastc_descriptor_memo_collisions_recompute_original_lookup() {
	mut slots := map[int]u32{}
	mut first := u32(0)
	mut second := u32(0)
	for i in 0 .. 65 {
		id := flat.source_type_id('MemoCollision${i}')
		slot := int(id & 63)
		if previous := slots[slot] {
			first = previous
			second = id
			break
		}
		slots[slot] = id
	}
	assert first != 0 && second != 0 && first != second
	mut names := flat.TypeTextBindings{}
	names.set(flat.source_type_name(first), 'memo.First')
	names.set(flat.source_type_name(second), 'memo.Second')
	mut aliases := flat.TypeTextBindings{}
	aliases.set(flat.source_type_name(first), 'int')
	aliases.set(flat.source_type_name(second), 'i64')
	g := fastc_descriptor_memo_test_parser(names, aliases, true)
	for id in [first, second, first] {
		assert g.semantic_type_key(id) == g.semantic_type_key_uncached(id)
		assert g.underlying_alias_type(id) == fastc_underlying_alias_type(id, aliases)
		assert g.type_descriptor_memo.semantic_keys[int(id & 63)].key == id
		assert g.type_descriptor_memo.aliases[int(id & 63)].key == id
	}
	assert g.semantic_type_key(first) == flat.source_type_id('memo.First')
	assert g.semantic_type_key(second) == flat.source_type_id('memo.Second')
	assert g.underlying_alias_type(first) == flat.source_type_int
	assert g.underlying_alias_type(second) == flat.source_type_i64
}

fn test_fastc_descriptor_memos_are_parser_local_and_module_independent() {
	mut names_a := flat.TypeTextBindings{}
	names_a.set('memo__Shared', 'memo.ContextA')
	mut names_b := flat.TypeTextBindings{}
	names_b.set('memo__Shared', 'memo.ContextB')
	mut aliases_a := flat.TypeTextBindings{}
	aliases_a.set('MemoSharedAlias', 'int')
	mut aliases_b := flat.TypeTextBindings{}
	aliases_b.set('MemoSharedAlias', 'i64')
	mut a := fastc_descriptor_memo_test_parser(names_a, aliases_a, true)
	b := fastc_descriptor_memo_test_parser(names_b, aliases_b, true)
	typ := flat.source_type_id('memo__Shared**')
	alias := flat.source_type_id('MemoSharedAlias*')
	assert a.semantic_type_key(typ) == flat.source_type_id('memo.ContextA')
	assert b.semantic_type_key(typ) == flat.source_type_id('memo.ContextB')
	assert a.underlying_alias_type(alias) == flat.source_type_id('int*')
	assert b.underlying_alias_type(alias) == flat.source_type_id('i64*')
	a.parse_mono_instance(FastcSourceFile{
		path:                 'descriptor_memo_context.v'
		source_offset:        137
		source_line_offset:   23
		source_column_offset: 9
		header:               FastcSourceHeader{
			module_name: 'different'

			imports: {
				'another': 'different.module'
			}
		}
	}) or { panic(err) }
	assert a.source_offset == 137
	assert a.source_line_offset == 23
	assert a.source_column_offset == 9
	assert a.semantic_type_key(typ) == flat.source_type_id('memo.ContextA')
	assert a.underlying_alias_type(alias) == flat.source_type_id('int*')
	assert a.type_descriptor_memo.semantic_keys[int(typ & 63)].key == typ
	assert a.type_descriptor_memo.aliases[int(alias & 63)].key == alias
}

fn test_fastc_descriptor_memos_preserve_zero_binding_semantics_without_caching() {
	mut names := flat.TypeTextBindings{}
	names.set('', 'memo.Zero')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('', 'int')
	g := fastc_descriptor_memo_test_parser(names, aliases, true)
	for _ in 0 .. 3 {
		assert g.semantic_type_key(0) == flat.source_type_id('memo.Zero')
		assert g.underlying_alias_type(0) == flat.source_type_int
	}
	assert g.type_descriptor_memo.semantic_keys[0].key == 0
	assert g.type_descriptor_memo.aliases[0].key == 0
}

fn fastc_descriptor_memo_checkpoint_read(checkpoint &Parser, typ u32) u32 {
	mut probe := *checkpoint
	probe.module_name = 'checkpoint'
	return probe.semantic_type_key(typ)
}

fn (mut g Parser) descriptor_memo_checkpoint_read(typ u32) u32 {
	checkpoint := g
	return fastc_descriptor_memo_checkpoint_read(&checkpoint, typ)
}

fn test_fastc_descriptor_memo_preserves_parser_checkpoint_value_copies() {
	mut names := flat.TypeTextBindings{}
	names.set('memo__Checkpoint', 'memo.Checkpoint')
	typ := flat.source_type_id('memo__Checkpoint*')
	for finalized in [false, true] {
		mut g := fastc_descriptor_memo_test_parser(names, flat.TypeTextBindings{}, finalized)
		assert g.descriptor_memo_checkpoint_read(typ) == flat.source_type_id('memo.Checkpoint')
		assert g.module_name == ''
		if finalized {
			// A checkpoint retains the same immutable descriptor context and may
			// share its file-local memo without changing any lookup result.
			assert g.type_descriptor_memo.semantic_keys[int(typ & 63)].key == typ
		} else {
			assert isnil(g.type_descriptor_memo)
		}
	}
}
