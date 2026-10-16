module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner

fn fastc_plain_member_parser(selfhost bool) Parser {
	prefs := pref.new_preferences()
	mut types := flat.TypeTextBindings{}
	types.set('count', 'int')
	types.set('short', 'int')
	types.set('text', 'string')
	types.set('next', 'Leaf*')
	types.set('items', 'Array_int')
	fields := {
		'Record': [
			FastcStructField{ name: 'count', typ: flat.source_type_int },
			FastcStructField{ name: 'short', typ: flat.source_type_int },
			FastcStructField{ name: 'text', typ: flat.source_type_string },
			FastcStructField{ name: 'next', typ: flat.source_type_id('Leaf*') },
			FastcStructField{ name: 'items', typ: flat.source_type_id('Array_int') },
			FastcStructField{ name: 'shared', typ: flat.source_type_id('Map_string_int'), is_shared_pointer: true },
			FastcStructField{ name: 'optional', typ: flat.source_type_id('Option'), option_value_type: flat.source_type_int },
			FastcStructField{ name: 'callback', typ: flat.source_type_voidptr, is_function: true },
			FastcStructField{ name: 'generic', typ: flat.source_type_voidptr, generic_argument_type: flat.source_type_int },
			FastcStructField{ name: 'channel', typ: flat.source_type_id('chan'), chan_element_type: flat.source_type_int },
			FastcStructField{ name: '__embedded_0', typ: flat.source_type_id('Leaf') },
		]
		'Leaf':   [FastcStructField{ name: 'promoted', typ: flat.source_type_int }]
	}
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Alias', 'Record')
	return Parser{
		prefs:               prefs
		s:                   scanner.new_scanner(prefs, .normal)
		selfhost:            selfhost
		module_name:         'fixture'
		struct_fields:       {
			'Record': types
		}
		struct_field_info:   fields
		struct_field_lookup: fastc_build_struct_field_lookup(fields)
		declared_kinds:      {
			'Record': .struct_
			'Leaf':   .struct_
			'Alias':  .alias_
			'Iface':  .interface_
			'Number': .enum_
			'Union':  .union_
		}
		alias_base_types:    aliases
		sum_types:           {
			'Choice': true
		}
		locals:              {
			'value':   FastcLocal{ typ: flat.source_type_id('Record') }
			'renamed': FastcLocal{ typ: flat.source_type_id('Record'), c_name: '__record_value' }
			// A local named like the field must not alter field storage.
			'next':    FastcLocal{ typ: flat.source_type_id('Leaf*') }
			'count':   FastcLocal{ typ: flat.source_type_id('Leaf*') }
		}
	}
}

fn fastc_plain_member_tokens(root string, member string) []FastcExpressionToken {
	return [FastcExpressionToken{ tok: .name, lit: root }, FastcExpressionToken{ tok: .dot, lit: '.' },
		FastcExpressionToken{ tok: .name, lit: member }]
}

fn test_fastc_plain_local_direct_members_preserve_legacy_dispatch_absence_and_output() {
	g := fastc_plain_member_parser(true)
	for root in ['value', 'renamed'] {
		for name in ['count', 'short', 'text', 'next', 'items'] {
			values := fastc_plain_member_tokens(root, name)
			assert g.is_plain_local_direct_member(values)
			raw := g.render_raw_expression_tokens(values) or { panic(name) }
			// An inert mut marker forces the full old dispatch. Every old branch
			// reading that marker also requires an ampersand token, absent here.
			mut legacy := values.clone()
			legacy[0] = FastcExpressionToken{ ...legacy[0], is_mut_argument: true }
			assert !g.is_plain_local_direct_member(legacy)
			assert g.render_special_expression(values, raw) == none
			assert g.render_special_expression(legacy, raw) == none
			typ := g.infer_expression_type(values) or { panic(name) }
			legacy_type := g.infer_expression_type(legacy) or { panic(name) }
			assert typ == legacy_type
			argument := g.render_call_argument_expression(values, typ) or { panic(name) }
			legacy_argument := g.render_call_argument_expression(legacy, typ) or { panic(name) }
			assert argument == legacy_argument
			membership := g.render_membership_candidate(values, typ) or { panic(name) }
			legacy_membership := g.render_membership_candidate(legacy, typ) or { panic(name) }
			assert membership == legacy_membership
			assert g.temp_id == 0
		}
	}
}

fn test_fastc_plain_local_direct_members_reject_special_storage_and_receivers() {
	mut g := fastc_plain_member_parser(true)
	for name in ['shared', 'optional', 'callback', 'generic', 'channel', 'promoted', 'absent'] {
		assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('value', name)), name
	}
	for type_name in ['Unknown', 'Alias', 'Iface', 'Number', 'Union', 'Choice', 'Option', '__v_result',
		'Record*', 'Array_Record', 'Map_string_Record', 'FixedArray_2_Record', 'voidptr', 'fn (int) int'] {
		g.locals['candidate'] = FastcLocal{ typ: flat.source_type_id(type_name) }
		assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('candidate', 'count')), type_name
	}
	for local in [
		FastcLocal{ typ: flat.source_type_id('Record'), is_mut: true },
		FastcLocal{ typ: flat.source_type_id('Record'), is_reference: true },
		FastcLocal{ typ: flat.source_type_id('Record'), option_value_type: flat.source_type_int },
		FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_type: flat.source_type_id('Choice') },
		FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_source: '__origin' },
	] {
		g.locals['candidate'] = local
		assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('candidate', 'count'))
	}
	for name in ['C', '@value', 'absent'] {
		assert !g.is_plain_local_direct_member(fastc_plain_member_tokens(name, 'count'))
	}
	mut values := fastc_plain_member_tokens('value', 'count')
	values[0] = FastcExpressionToken{ ...values[0], source: '__rendered' }
	assert !g.is_plain_local_direct_member(values)
	values = fastc_plain_member_tokens('value', 'count')
	values[0] = FastcExpressionToken{ ...values[0], typ: flat.source_type_id('Leaf') }
	assert !g.is_plain_local_direct_member(values)
	values = fastc_plain_member_tokens('value', 'count')
	values[2] = FastcExpressionToken{ ...values[2], typ: flat.source_type_int }
	assert !g.is_plain_local_direct_member(values)
	g.member_smartcasts['other'] = FastcMemberSmartcast{ typ: flat.source_type_int, source: '__narrowed' }
	assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('value', 'count'))
	g.member_smartcasts.clear()
	g.imports['value'] = 'some.module'
	assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('value', 'count'))
	g.imports.clear()
	g.in_mono_drain = true
	assert !g.is_plain_local_direct_member(fastc_plain_member_tokens('value', 'count'))
	g.in_mono_drain = false
	ordinary := fastc_plain_member_parser(false)
	assert !ordinary.is_plain_local_direct_member(fastc_plain_member_tokens('value', 'count'))
}

fn test_fastc_plain_local_direct_members_keep_runtime_field_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_plain_members_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Leaf { count int }
struct Record {
	count int
	short int
	text string
	next &Leaf
	items []int
}
fn read(value Record) int {
	count := &Leaf{count: 17}
	assert count.count == 17
	next := &Leaf{count: 99}
	assert next.count == 99
	assert value.text == "value"
	assert value.next.count == 5
	assert value.items[0] == 7
	return value.count + value.short
}
fn main() {
	value := Record{count: 2, short: 3, text: "value", next: &Leaf{count: 5}, items: [7]}
	assert read(value) == 5
	assert value.count == 2
	assert value.short == 3
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
