module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner

fn fastc_aggregate_argument_parser() Parser {
	prefs := pref.new_preferences()
	mut names := flat.TypeTextBindings{}
	names.set('fixture__Record', 'fixture.Record')
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Alias', 'fixture__Record')
	aliases.set('ArrayAlias', 'Array_int')
	aliases.set('MapAlias', 'Map_string_int')
	return Parser{
		prefs:                 prefs
		s:                     scanner.new_scanner(prefs, .normal)
		selfhost:              true
		module_name:           'fixture'
		declared_type_c_names: names
		alias_base_types:      aliases
		declared_kinds:        {
			'fixture.Record':   .struct_
			'Empty':            .struct_
			'Alias':            .alias_
			'ArrayAlias':       .alias_
			'MapAlias':         .alias_
			'Callable':         .alias_
			'Choice':           .alias_
			'Iface':            .interface_
			'Number':           .enum_
			'Union':            .union_
			'Option':           .struct_
			'__v_result':       .struct_
			'FixedArray_2_int': .struct_
		}
		sum_types:             {
			'Choice': true
		}
		locals:                {
			'record':        FastcLocal{ typ: flat.source_type_id('fixture__Record') }
			'renamed':       FastcLocal{ typ: flat.source_type_id('fixture__Record'), c_name: '__record_copy' }
			'short':         FastcLocal{ typ: flat.source_type_id('fixture__Record') }
			'mutable':       FastcLocal{ typ: flat.source_type_id('fixture__Record'), is_mut: true }
			'empty':         FastcLocal{ typ: flat.source_type_id('Empty') }
			'array':         FastcLocal{ typ: flat.source_type_id('Array_fixture__Record') }
			'nested_array':  FastcLocal{ typ: flat.source_type_id('Array_Array_int') }
			'pointer_array': FastcLocal{ typ: flat.source_type_id('Array_fixture__Record_ptr') }
			'map':           FastcLocal{ typ: flat.source_type_id('Map_string_fixture__Record') }
			'nested_map':    FastcLocal{ typ: flat.source_type_id('Map_u64_Array_fixture__Record') }
			'reference':     FastcLocal{ typ: flat.source_type_id('fixture__Record'), is_reference: true }
			'optional':      FastcLocal{ typ: flat.source_type_id('fixture__Record'), option_value_type: flat.source_type_id('fixture__Record') }
			'origin':        FastcLocal{ typ: flat.source_type_id('fixture__Record'), smartcast_origin_type: flat.source_type_id('Choice'), smartcast_origin_source: '__boxed_record' }
			'C':             FastcLocal{ typ: flat.source_type_id('fixture__Record') }
		}
		// Local names must keep precedence over matching function/constant names.
		functions:             {
			'fixture.record': FastcFunctionSignature{}
		}
		constants:             {
			'fixture.record': '__constant_record'
		}
	}
}

fn test_fastc_aggregate_local_arguments_match_complete_legacy_dispatch() {
	g := fastc_aggregate_argument_parser()
	for name in ['record', 'renamed', 'short', 'mutable', 'empty', 'array', 'nested_array',
		'pointer_array', 'map', 'nested_map'] {
		local := g.locals[name] or { panic(name) }
		for token_type in [u32(0), local.typ] {
			value := FastcExpressionToken{ tok: .name, lit: name, typ: token_type }
			direct := g.render_selfhost_scalar_call_argument([value], local.typ) or { panic(name) }
			fast := g.render_call_argument_expression([value], local.typ) or { panic(name) }
			// Only the shortcut reads this flag for an exact-type, nonpointer
			// singleton. The later mut-local branch requires expected == typ + '*',
			// so setting it forces the complete old pipeline with identical semantics.
			legacy_value := FastcExpressionToken{ tok: .name, lit: name, typ: token_type, is_mut_argument: true }
			assert g.render_selfhost_scalar_call_argument([legacy_value], local.typ) == none
			legacy := g.render_call_argument_expression([legacy_value], local.typ) or { panic(name) }
			assert direct == legacy, name
			assert fast == legacy, name
			assert fast == if local.c_name != '' { local.c_name } else { fastc_c_identifier(name) }
			assert g.temp_id == 0
			assert g.composite_types.len == 0 && g.fixed_array_types.ids.len == 0
		}
	}
}

fn test_fastc_aggregate_local_arguments_reject_coercions_and_special_storage() {
	mut g := fastc_aggregate_argument_parser()
	for type_name in ['Unknown', 'Alias', 'ArrayAlias', 'MapAlias', 'Callable', 'fn (int) int',
		'Choice', 'Iface', 'Number', 'Union', 'Option', '__v_result', 'FixedArray_2_int',
		'fixture__Record*', 'Array_int*', 'Map_string_int*', 'voidptr'] {
		typ := flat.source_type_id(type_name)
		g.locals['value'] = FastcLocal{ typ: typ }
		assert g.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: 'value' }], typ) == none, type_name
	}
	for name in ['reference', 'optional', 'origin', 'C', 'absent'] {
		assert g.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: name }], flat.source_type_id('fixture__Record')) == none, name
	}
	for value in [
		FastcExpressionToken{ tok: .name, lit: 'record', is_mut_argument: true },
		FastcExpressionToken{ tok: .name, lit: 'record', source: fastc_token_source('__pre_rendered'), typ: flat.source_type_id('fixture__Record') },
		FastcExpressionToken{ tok: .name, lit: 'record', typ: flat.source_type_id('Empty') },
		FastcExpressionToken{ tok: .number, lit: '0' },
	] {
		assert g.render_selfhost_scalar_call_argument([value], flat.source_type_id('fixture__Record')) == none
	}
	assert g.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: 'record' }], flat.source_type_id('Empty')) == none
	assert g.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: 'record' }], u32(0)) == none
	g.member_smartcasts['record'] = FastcMemberSmartcast{ typ: flat.source_type_id('fixture__Record'), source: '__narrowed_record' }
	assert g.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: 'record' }], flat.source_type_id('fixture__Record')) == none
	ordinary := Parser{ prefs: g.prefs, s: g.s, locals: g.locals, declared_kinds: g.declared_kinds, declared_type_c_names: g.declared_type_c_names }
	assert ordinary.render_selfhost_scalar_call_argument([FastcExpressionToken{ tok: .name, lit: 'record' }], flat.source_type_id('fixture__Record')) == none
}

fn test_fastc_aggregate_call_arguments_preserve_values_from_field_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_aggregate_arguments_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Record {
mut:
	count int
	items []int
	entries map[string]int
}
struct Holder {
	record Record
	items []Record
	entries map[string]Record
}
fn read_record(value Record) int {
	mut copy := value
	copy.count += 10
	return copy.count + copy.items[0] + copy.entries["entry"]
}
fn read_array(values []Record) int {
	return read_record(values[0])
}
fn read_map(values map[string]Record) int {
	return read_record(values["record"])
}
fn read_nested(values [][]Record) int {
	return read_array(values[0])
}
fn from_reference(mut value Record) int {
	return read_record(value)
}
fn main() {
	holder := Holder{
		record: Record{count: 2, items: [3], entries: {"entry": 5}}
		items: [Record{count: 2, items: [3], entries: {"entry": 5}}]
		entries: {"record": Record{count: 2, items: [3], entries: {"entry": 5}}}
	}
	mut record := holder.record
	items := holder.items
	entries := holder.entries
	nested := [items]
	assert read_record(record) == 20
	assert read_array(items) == 20
	assert read_map(entries) == 20
	assert read_nested(nested) == 20
	assert from_reference(mut record) == 20
	assert record.count == 2
	assert record.items[0] == 3
	assert record.entries["entry"] == 5
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
