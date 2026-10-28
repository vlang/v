module transform

import os
import v.flat
import v.gen.c as cgen
import v.parser
import v.pref
import v.types

fn storage_borrow_fixture(mut t Transformer, name string, params []types.Type, summary []bool) {
	decl := t.a.add_node(flat.Node{ kind: .fn_decl, value: name })
	t.call_param_types_index_ready = true
	t.call_param_types_decl_index[name] = FnParamDeclRef{ idx: int(decl) }
	t.call_param_types_decl_cache[int(decl)] = params
	t.fixed_array_borrow_params[int(decl)] = summary
	t.tc.fn_param_types[name] = params
	t.tc.fn_ret_types[name] = types.Type(types.Pointer{ base_type: types.Type(types.int_) })
}

fn test_borrowed_storage_address_keeps_payload_aliases_and_self_references() {
	for proven in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
		storage_borrow_fixture(mut t, 'borrow', [ptr], [proven])
		local := t.make_ident('local')
		tc.register_synth_type(local, types.Type(types.int_))
		address := t.make_prefix(.amp, local)
		tc.register_synth_type(address, ptr)
		call := t.make_call_typed('borrow', [address], flat.source_type_id('&int'))
		tc.register_synth_type(call, ptr)
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == if proven {
			[]string{}
		} else {
			['local']
		}
		// A copied aggregate can forward an embedded pointer despite borrowing its header.
		assert t.escape_aggregate_address_sources(call, {
			'local': ['pointee']
		}, map[string]string{}) == if proven {
			['pointee']
		} else {
			['pointee', 'local']
		}
		assert t.escape_aggregate_address_sources(call, {
			'local': ['local']
		}, map[string]string{}) == ['local']
	}
}

fn test_borrowed_storage_only_strips_the_explicit_address_layer() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
	storage_borrow_fixture(mut t, 'borrow', [ptr], [true])
	owner := t.make_ident('owner')
	tc.register_synth_type(owner, types.Type(types.Struct{ name_id: flat.source_type_id('Owner') }))
	field := t.make_selector(owner, 'pointer', flat.source_type_id('&int'))
	tc.register_synth_type(field, ptr)
	address := t.make_prefix(.amp, field)
	tc.register_synth_type(address, types.Type(types.Pointer{ base_type: ptr }))
	call := t.make_call_typed('borrow', [address], flat.source_type_id('&int'))
	tc.register_synth_type(call, ptr)
	assert t.escape_aggregate_address_sources(call, {
		'owner': ['pointee']
	}, map[string]string{}) == ['pointee', 'owner']
	// A pointer alias is not an explicit address. Keep its existing conservative provenance.
	pointer := t.make_ident('p')
	tc.register_synth_type(pointer, ptr)
	alias_call := t.make_call_typed('borrow', [pointer], flat.source_type_id('&int'))
	tc.register_synth_type(alias_call, ptr)
	assert t.escape_aggregate_address_sources(alias_call, {
		'p': ['local']
	}, map[string]string{}) == ['local']
}

fn test_borrowed_storage_dereference_and_scalar_fields_keep_original_provenance() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
	storage_borrow_fixture(mut t, 'borrow', [ptr], [true])
	pointer := t.make_ident('pointer')
	tc.register_synth_type(pointer, ptr)
	deref := t.make_prefix(.mul, pointer)
	tc.register_synth_type(deref, types.Type(types.int_))
	owner := t.make_ident('owner')
	tc.register_synth_type(owner, types.Type(types.Struct{ name_id: flat.source_type_id('Owner') }))
	field := t.make_selector(owner, 'encoded', flat.source_type_id('int'))
	tc.register_synth_type(field, types.Type(types.int_))
	for operand in [deref, field] {
		address := t.make_prefix(.amp, operand)
		tc.register_synth_type(address, ptr)
		call := t.make_call_typed('borrow', [address], flat.source_type_id('&int'))
		tc.register_synth_type(call, ptr)
		assert t.escape_aggregate_address_sources(call, {
			'pointer': ['stack']
			'owner':   ['stack']
		}, map[string]string{}) == if operand == deref {
			['stack']
		} else {
			['stack', 'owner']
		}
	}
	// An identifier's recorded provenance also covers pointer-encoded scalars.
	encoded := t.make_ident('encoded')
	tc.register_synth_type(encoded, types.Type(types.int_))
	address := t.make_prefix(.amp, encoded)
	tc.register_synth_type(address, ptr)
	call := t.make_call_typed('borrow', [address], flat.source_type_id('&int'))
	tc.register_synth_type(call, ptr)
	assert t.escape_aggregate_address_sources(call, {
		'encoded': ['stack']
	}, map[string]string{}) == ['stack']
}

fn test_unknown_function_values_arity_and_nested_calls_stay_conservative() {
	for mode in ['known', 'unknown', 'fn_value', 'lexical_shadow', 'arity', 'nested'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
		if mode != 'unknown' {
			storage_borrow_fixture(mut t, 'borrow', [ptr], [true])
		}
		if mode == 'fn_value' {
			t.set_var_type('borrow', flat.source_type_id('fn (&int) &int'))
		}
		local := t.make_ident('local')
		tc.register_synth_type(local, types.Type(types.int_))
		address := t.make_prefix(.amp, local)
		tc.register_synth_type(address, ptr)
		mut args := [address]
		if mode == 'arity' { args << address }
		mut call := t.make_call_typed('borrow', args, flat.source_type_id('&int'))
		tc.register_synth_type(call, ptr)
		if mode == 'lexical_shadow' {
			shadow := t.make_decl_assign('borrow', t.make_ident('callback'))
			body := t.make_block([shadow, call])
			start := a.children.len
			a.children << body
			a.add_node(flat.Node{ kind: .fn_decl, value: 'caller', children_start: start, children_count: 1 })
		}
		if mode == 'nested' {
			call = t.make_call_typed('unknown_outer', [call], flat.source_type_id('&int'))
			tc.register_synth_type(call, ptr)
		}
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == if mode in [
			'known',
			'nested',
		] {
			[]string{}
		} else {
			['local']
		}, mode
	}
}

fn test_reference_receiver_borrows_only_its_storage_and_dynamic_interfaces_are_conservative() {
	for is_interface in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		receiver_type := if is_interface {
			types.Type(types.Interface{ name_id: flat.source_type_id('Owner') })
		} else {
			types.Type(types.Struct{ name_id: flat.source_type_id('Owner') })
		}
		ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
		storage_borrow_fixture(mut t, 'Owner.read', [
			types.Type(types.Pointer{ base_type: receiver_type }),
			ptr,
		], [true, true])
		receiver := t.make_ident('owner')
		t.set_var_type('owner', flat.source_type_id('Owner'))
		tc.register_synth_type(receiver, receiver_type)
		callee := t.make_selector(receiver, 'read', 0)
		local := t.make_ident('local')
		tc.register_synth_type(local, types.Type(types.int_))
		address := t.make_prefix(.amp, local)
		tc.register_synth_type(address, ptr)
		start := a.children.len
		a.children << [callee, address]
		call := a.add_node(flat.Node{ kind: .call, children_start: start, children_count: 2 })
		tc.register_synth_type(call, ptr)
		t.set_resolved_call_entry(int(call), 'Owner.read')
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == if is_interface {
			['local']
		} else {
			[]string{}
		}
		assert t.escape_aggregate_address_sources(call, {
			'owner': ['payload']
		}, map[string]string{}) == if is_interface {
			['payload', 'local']
		} else {
			['payload']
		}
	}
}

const storage_borrow_source = 'module main
struct Entry { value int }
struct Tree { entries []Entry }
struct Payload { pointer &int }
struct Holder { inner Payload }
fn extract(copy Payload) &int { return copy.pointer }
fn (holder &Holder) copy_payload() &int { return extract(holder.inner) }
fn payload_snapshot(value int) &int {
 local := value
 holder := Holder{inner: Payload{pointer: &local}}
 return holder.copy_payload()
}
fn (tree &Tree) index(node &Entry) int { return node.value }
fn (tree &Tree) child_node(node &Entry) &Entry {
 id := tree.index(node)
 return unsafe { &tree.entries[id] }
}
fn (tree &Tree) type_reference(node &Entry) &Entry {
 if node.value == 0 { return unsafe { &tree.entries[0] } }
 return unsafe { &tree.entries[1] }
}
fn (tree &Tree) copied_payload(node &Entry) &Entry {
 _ = node.value
 return tree.child_node(node)
}
fn identity(node &Entry) &Entry { return node }
fn inline_address(node &Entry) &int { return &node.value }
fn snapshot_lookup(tree &Tree, node Entry) &Entry { return tree.child_node(&node) }
fn snapshot_type(tree &Tree, node Entry) &Entry { return tree.type_reference(&node) }
fn snapshot_identity(node Entry) &Entry { return identity(&node) }
fn main() {}
'

fn test_borrowed_snapshot_pointer_results_remove_boxes_but_identity_remains_boxed() {
	path := os.join_path(os.vtmp_dir(), 'return_storage_borrow_${os.getpid()}.v')
	os.write_file(path, storage_borrow_source)!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	t.cur_module = 'main'
	t.cur_file = path
	t.transformed_fns = []bool{len: a.nodes.len}
	t.prepare()
	mut used := map[string]bool{}
	for i, node in a.nodes {
		if node.kind == .fn_decl {
			used[node.value] = true
			if node.value in ['Tree.child_node', 'Tree.type_reference', 'Tree.copied_payload'] {
				assert t.fixed_array_borrow_params[i] == [false, true], '${node.value}: ${t.fixed_array_borrow_params[i]}'
			}
			if node.value == 'Holder.copy_payload' {
				assert t.fixed_array_borrow_params[i] == [true]
			}
			if node.value in ['identity', 'inline_address'] {
				assert t.fixed_array_borrow_params[i].len == 0
			}
		}
	}
	t.transform_all()
	assert t.monomorph_errors.len == 0, t.monomorph_errors.str()
	mut gen := cgen.FlatGen.new()
	generated := gen.gen_with_used(a, used, &tc)
	for name in ['snapshot_lookup', 'snapshot_type'] {
		assert generated.contains('${name}(')
		body := generated.all_after_last('${name}(').all_after('{').all_before('\n}')
		assert !body.contains('memdup('), '${name}: ${body}'
		assert !body.contains('__heap_value_param'), '${name}: ${body}'
	}
	identity_body := generated.all_after_last('snapshot_identity(').all_after('{').all_before('\n}')
	assert identity_body.contains('memdup('), identity_body
	payload_body := generated.all_after_last('payload_snapshot(').all_after('{').all_before('\n}')
	assert payload_body.contains('memdup('), payload_body
}
