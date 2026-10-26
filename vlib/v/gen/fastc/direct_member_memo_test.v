module fastc

import v.flat
import v.pref
import v.scanner

fn direct_member40_parser(finalized bool) Parser {
	prefs := pref.new_preferences()
	mut record := flat.TypeTextBindings{}
	record.set('value', 'int')
	record.set('', 'bool')
	record.set('later', '')
	mut other := flat.TypeTextBindings{}
	other.set('value', 'string')
	mut array_fields := flat.TypeTextBindings{}
	array_fields.set('len', 'int')
	array_fields.set('data', 'voidptr')
	mut map_fields := flat.TypeTextBindings{}
	map_fields.set('key_values', 'voidptr')
	mut zero := flat.TypeTextBindings{}
	zero.set('value', 'i64')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('AliasRecord', 'MemoRecord')
	aliases.set('AliasMap', 'Map_string_int')
	mut names := flat.TypeTextBindings{}
	names.set('memo__Face', 'memo.Face')
	return Parser{
		prefs:                 prefs
		s:                     scanner.new_scanner(prefs, .normal)
		selfhost:              true
		module_name:           'memo'
		alias_base_types:      aliases
		declared_type_c_names: names
		declared_kinds:        {
			'memo.Face':   .interface_
			'AliasRecord': .alias_
			'AliasMap':    .alias_
		}
		struct_fields:         {
			'MemoRecord': record
			'MemoOther':  other
			'array':      array_fields
			'map':        map_fields
			'':           zero
		}
		interface_fields:      {
			'memo.Face.value': FastcInterfaceField{ name: 'value', typ: flat.source_type_bool }
		}
		type_descriptor_memo:  if finalized { &FastcTypeDescriptorMemo{} } else { unsafe { nil } }
	}
}

fn test_direct_member40_matches_original_for_struct_pointer_alias_interface_and_descriptors() {
	g := direct_member40_parser(true)
	cases := {
		'MemoRecord':     {
			'value':   flat.source_type_int
			'':        flat.source_type_bool
			'missing': u32(0)
		}
		'MemoRecord**':   {
			'value':   flat.source_type_int
			'missing': u32(0)
		}
		'MemoOther':      {
			'value': flat.source_type_string
		}
		'AliasRecord':    {
			'value': flat.source_type_int
		}
		'Array_int':      {
			'len':  flat.source_type_int
			'data': flat.source_type_voidptr
		}
		'Array_int*':     {
			'len':     flat.source_type_int
			'missing': u32(0)
		}
		'Map_string_int': {
			'len':        flat.source_type_int
			'key_values': flat.source_type_voidptr
		}
		'AliasMap*':      {
			'len': flat.source_type_int
		}
		'memo__Face*':    {
			'value':   flat.source_type_bool
			'missing': u32(0)
		}
		'MemoUnknown':    {
			'missing': u32(0)
		}
	}
	for receiver, fields in cases {
		typ := flat.source_type_id(receiver)
		for field, expected in fields {
			for _ in 0 .. 3 {
				assert g.struct_direct_member_type(typ, field) == g.struct_direct_member_type_uncached(typ, field), '${receiver}.${field}'
				assert g.struct_direct_member_type(typ, field) == expected, '${receiver}.${field}'
				assert g.type_descriptor_memo.direct_member_known
				assert g.type_descriptor_memo.direct_member_receiver == typ
				assert g.type_descriptor_memo.direct_member_field == field
				assert g.type_descriptor_memo.direct_member_type == expected
			}
		}
	}
}

fn test_direct_member40_zero_receiver_bypasses_even_a_live_positive_or_negative_slot() {
	g := direct_member40_parser(true)
	typ := flat.source_type_id('MemoRecord')
	for field in ['value', 'missing'] {
		expected := g.struct_direct_member_type(typ, field)
		assert g.type_descriptor_memo.direct_member_known
		for _ in 0 .. 2 {
			assert g.struct_direct_member_type(0, 'value') == flat.source_type_i64
			assert g.struct_direct_member_type(0, 'value') == g.struct_direct_member_type_uncached(0, 'value')
			assert g.type_descriptor_memo.direct_member_receiver == typ
			assert g.type_descriptor_memo.direct_member_field == field
			assert g.type_descriptor_memo.direct_member_type == expected
		}
	}
}

fn test_direct_member40_arbitrary_parser_keeps_mutable_descriptors_uncached() {
	g := direct_member40_parser(false)
	typ := flat.source_type_id('MemoRecord')
	assert g.struct_direct_member_type(typ, 'value') == flat.source_type_int
	assert g.struct_direct_member_type(typ, 'later') == 0
	mut fields := g.struct_fields['MemoRecord'] or { panic('missing fixture fields') }
	fields.set('value', 'string')
	fields.set('later', 'bool')
	assert g.struct_direct_member_type(typ, 'value') == flat.source_type_string
	assert g.struct_direct_member_type(typ, 'later') == flat.source_type_bool
	assert isnil(g.type_descriptor_memo)
}

fn direct_member40_checkpoint_read(checkpoint &Parser, typ u32, field string) u32 {
	mut probe := *checkpoint
	probe.module_name = 'checkpoint'
	return probe.struct_direct_member_type(typ, field)
}

fn (mut g Parser) direct_member40_checkpoint_read(typ u32, field string) u32 {
	checkpoint := g
	return direct_member40_checkpoint_read(&checkpoint, typ, field)
}

fn test_direct_member40_checkpoint_context_switch_and_separate_memos_keep_original_results() {
	mut g := direct_member40_parser(true)
	other := direct_member40_parser(true)
	typ := flat.source_type_id('MemoRecord')
	assert g.direct_member40_checkpoint_read(typ, 'value') == flat.source_type_int
	assert g.module_name == 'memo'
	assert g.type_descriptor_memo.direct_member_receiver == typ
	assert g.type_descriptor_memo.direct_member_field == 'value'
	assert !other.type_descriptor_memo.direct_member_known
	assert other.struct_direct_member_type(flat.source_type_id('MemoOther'), 'value') == flat.source_type_string
	assert g.type_descriptor_memo.direct_member_receiver == typ
	g.parse_mono_instance(FastcSourceFile{
		path:   'direct_member_context.v'
		header: FastcSourceHeader{
			module_name: 'different'
			imports:     {
				'another': 'another.module'
			}
		}
	}) or { panic(err) }
	assert g.struct_direct_member_type(typ, 'value') == flat.source_type_int
	assert g.struct_direct_member_type(typ, 'value') == g.struct_direct_member_type_uncached(typ, 'value')
	assert voidptr(g.type_descriptor_memo) != voidptr(other.type_descriptor_memo)
}
