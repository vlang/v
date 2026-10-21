module transform

import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_scalar_result_provenance_summary_requires_local_consumption() {
	path := os.join_path(os.vtmp_dir(), 'scalar_result_borrow_${os.getpid()}.v')
	os.write_file(path, 'module main

type Success = bool
type Narrow = i16
struct Holder {
 value i16
mut:
 pointer &i16 = unsafe { nil }
}
fn read(value &i16) !i16 { return unsafe { *value } }
fn read_length(value []int) !i16 { return i16(value.len) }
fn read_item(value &[2]u8) !i16 { return i16(value[0]) }
fn (holder Holder) read_value() !i16 { return holder.value }
fn read_alias(value &i16) !Success { return unsafe { *value != 0 } }
fn forward(value &i16) !i16 { return read(value) }
fn alias(value &i16) !i16 { other := value; return unsafe { *other } }
fn (mut holder Holder) retain(value &i16) !bool {
 holder.pointer = value
 return true
}
fn narrow_encoded(value &usize) !bool { return unsafe { *value != 0 } }
fn encoded(value &i16) !usize { return usize(value) }
fn alias_math(value &Narrow) !i16 { return unsafe { i16(*value + *value) } }
fn main() {}
')!
	defer { os.rm(path) or {} }
	for parallel in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.parallel_enabled = parallel
		t.skip_generics = true
		t.scope_parallel_workers = true
		t.retain_prescan_scopes = true
		t.prepare_with_pre_scans()
		mut summaries := map[string][]bool{}
		for i, node in a.nodes {
			if node.kind == .fn_decl {
				summaries[node.value] = (t.call_provenance_borrow_params[i] or { []bool{} }).clone()
			}
		}
		for name in ['read', 'read_length', 'read_item', 'Holder.read_value', 'read_alias',
			'narrow_encoded'] {
			assert summaries[name] == [true], '${name}: ${summaries[name]}'
		}
		for name in ['forward', 'alias', 'alias_math'] {
			assert summaries[name] == [false], '${name}: ${summaries[name]}'
		}
		assert summaries['Holder.retain'] == [false, false]
		assert summaries['encoded'] == [true]
		for scope in t.prescan_scopes {
			transform_worker_scope_free(scope)
		}
	}
}

fn test_gc_statistics_borrow_exception_requires_the_exact_direct_c_call() {
	for mode in ['c', 'local', 'method', 'wrong_arity'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		base := t.make_ident(if mode == 'method' { 'value' } else { 'C' })
		callee := if mode == 'local' {
			t.make_ident('GC_get_heap_usage_safe')
		} else {
			t.make_selector(base, 'GC_get_heap_usage_safe', 0)
		}
		argument := t.make_prefix(.amp, t.make_ident('stats'))
		start := a.children.len
		a.children << callee
		for _ in 0 .. if mode == 'wrong_arity' { 4 } else { 5 } {
			a.children << argument
		}
		call := a.add_node(flat.Node{
			kind:           .call
			children_start: start
			children_count: a.children.len - start
		})
		t.set_resolved_call_entry(int(call), if mode == 'local' {
			'GC_get_heap_usage_safe'
		} else if mode == 'method' {
			'Stats.GC_get_heap_usage_safe'
		} else {
			'C.GC_get_heap_usage_safe'
		})
		assert t.call_provenance_known_borrow(call, *a.node(call)) == (mode == 'c'), mode
	}
}
