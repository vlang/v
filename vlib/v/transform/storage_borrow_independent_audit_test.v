module transform

import os
import v.flat
import v.gen.c as cgen
import v.parser
import v.pref
import v.types

const storage_borrow_audit_source = 'module main
struct AuditPayload { pointer &int }
struct AuditEnvelope { inner AuditPayload }
struct AuditSink { mut: pointer &int = unsafe { nil } }
fn audit_extract(copy AuditPayload) &int { return copy.pointer }
fn audit_extract_optional(copy AuditPayload) ?&int { return copy.pointer }
fn audit_extract_result(copy AuditPayload) !&int { return copy.pointer }
fn audit_storage(input &AuditEnvelope) &int { return audit_extract(input.inner) }
fn audit_storage_optional(input &AuditEnvelope) ?&int { return audit_extract_optional(input.inner) }
fn audit_storage_result(input &AuditEnvelope) !&int { return audit_extract_result(input.inner) }
fn audit_storage_field(input &AuditEnvelope) &&int { return &input.inner.pointer }
fn audit_store_payload(input &AuditEnvelope, mut sink AuditSink) &int {
 sink.pointer = input.inner.pointer
 return sink.pointer
}
fn audit_direct(value int) &int {
 local := value
 payload := AuditEnvelope{inner: AuditPayload{pointer: &local}}
 return audit_storage(&payload)
}
fn audit_optional(value int) ?&int {
 local := value
 payload := AuditEnvelope{inner: AuditPayload{pointer: &local}}
 return audit_storage_optional(&payload)
}
fn audit_result(value int) !&int {
 local := value
 payload := AuditEnvelope{inner: AuditPayload{pointer: &local}}
 return audit_storage_result(&payload)
}
fn audit_field(value int) &&int {
 local := value
 payload := AuditEnvelope{inner: AuditPayload{pointer: &local}}
 return audit_storage_field(&payload)
}
fn audit_mutable_store(value int, mut sink AuditSink) &int {
 local := value
 payload := AuditEnvelope{inner: AuditPayload{pointer: &local}}
 return audit_store_payload(&payload, mut sink)
}
fn main() {}
'

fn test_storage_only_summary_keeps_real_embedded_aliases_through_return_wrappers() {
	path := os.join_path(os.vtmp_dir(), 'storage_borrow_audit_${os.getpid()}.v')
	os.write_file(path, storage_borrow_audit_source)!
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
		if node.kind != .fn_decl {
			continue
		}
		used[node.value] = true
		if node.value in ['audit_storage', 'audit_storage_optional', 'audit_storage_result'] {
			assert t.fixed_array_borrow_params[i] == [true], '${node.value}: ${t.fixed_array_borrow_params[i]}'
		}
		if node.value in ['audit_storage_field', 'audit_store_payload'] {
			summary := t.fixed_array_borrow_params[i] or { []bool{} }
			assert summary.len == 0 || !summary[0], '${node.value}: ${summary}'
		}
	}
	t.transform_all()
	assert t.monomorph_errors.len == 0, t.monomorph_errors.str()
	mut gen := cgen.FlatGen.new()
	generated := gen.gen_with_used(a, used, &tc)
	for name in ['audit_direct', 'audit_optional', 'audit_result', 'audit_field', 'audit_mutable_store'] {
		assert generated.contains('${name}('), name
		body := generated.all_after_last('${name}(').all_after('{').all_before('\n}')
		mut local_is_boxed := false
		for line in body.split_into_lines() {
			if line.contains('local =') && line.contains('memdup(') {
				local_is_boxed = true
			}
		}
		assert local_is_boxed, '${name}: ${body}'
	}
	proof := os.getenv('STORAGE_BORROW_AUDIT_C')
	if proof.len > 0 {
		os.write_file(proof, generated)!
	}
}

fn test_storage_projection_rejects_nonidentifier_address_operands() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	value := t.make_ident('value')
	assert t.escape_storage_borrow_ident(value)
	paren := t.make_paren(value)
	assert t.escape_storage_borrow_ident(paren)
	field := t.make_selector(value, 'field', 0)
	assert !t.escape_storage_borrow_ident(field)
	deref := t.make_prefix(.mul, value)
	assert !t.escape_storage_borrow_ident(deref)
}

fn test_storage_summary_rejects_wrapped_indirect_callees() {
	for kind in [flat.NodeKind.index, .paren] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		ptr := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
		decl := a.add_node(flat.Node{ kind: .fn_decl, value: 'audit_borrow' })
		t.call_param_types_index_ready = true
		t.call_param_types_decl_index['audit_borrow'] = FnParamDeclRef{ idx: int(decl) }
		t.call_param_types_decl_cache[int(decl)] = [ptr]
		t.fixed_array_borrow_params[int(decl)] = [true]
		tc.fn_param_types['audit_borrow'] = [ptr]
		tc.fn_ret_types['audit_borrow'] = ptr
		callback := t.make_ident('audit_borrow')
		t.set_var_type('audit_borrow', flat.source_type_id('fn (&int) &int'))
		wrapped_start := a.children.len
		a.children << callback
		wrapped := a.add_node(flat.Node{
			kind:           kind
			children_start: wrapped_start
			children_count: 1
		})
		local := t.make_ident('local')
		tc.register_synth_type(local, types.Type(types.int_))
		address := t.make_prefix(.amp, local)
		tc.register_synth_type(address, ptr)
		call_start := a.children.len
		a.children << [wrapped, address]
		call := a.add_node(flat.Node{
			kind:           .call
			children_start: call_start
			children_count: 2
		})
		tc.register_synth_type(call, ptr)
		t.set_resolved_call_entry(int(call), 'audit_borrow')
		assert !t.escape_call_storage_borrow(call, *a.node(call)).borrows(1), kind.str()
		assert t.escape_aggregate_address_sources(call, map[string][]string{}, map[string]string{}) == ['local'], kind.str()
	}
}
