module types

import os
import v3.flat
import v3.parser
import v3.pref

fn test_param_storage_sources_include_all_delegated_mutable_targets() {
	path := os.join_path(os.vtmp_dir(), 'v3_delegated_storage_sources_${os.getpid()}.v')

	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

struct Pair {
mut:
	first  &Box
	second &Box
}

struct IndexedBox {
mut:
	values []&Value
}

fn route_targets(mut first &Box, mut second &Box, value &Value) {
	_ = first
	second.value = value
}

fn set_pair_sources(mut pair Pair, value &Value) {
	route_targets(mut pair.first, mut pair.second, value)
}

fn pick_index() int {
	return 0
}

fn store_at_computed_index(mut box IndexedBox, value &Value) {
	box.values[pick_index()] = value
}

fn copy_then_store(mut box Box, value &Value) {
	mut copy := box
	copy.value = value
}

fn set_or_reset(mut box &Box, value &Value, replacement &Value, stop bool) {
	box.value = value
	if stop {
		return
	}
	box.value = replacement
}

fn store_value(mut box &Box, value &Value) {
	box.value = value
}

fn delegate_then_reset(mut box &Box, value &Value, replacement &Value) {
	store_value(mut box, value)
	box.value = replacement
}

fn store_in_loop(mut box &Box, value &Value, replacement &Value, stop bool) {
	for {
		box.value = value
		if stop {
			break
		}
		box.value = replacement
		break
	}
}

fn may_fail(ok bool) ! {
	if !ok {
		return error("failed")
	}
}

fn store_or_replace(mut box &Box, value &Value, replacement &Value, ok bool) {
	box.value = value
	may_fail(ok) or {
		box.value = replacement
	}
}

fn main() {
	mut value := Value{}
	mut first := &Box{
		value: &value
	}
	mut second := &Box{
		value: &value
	}
	mut pair := Pair{
		first: first
		second: second
	}
	mut indexed := IndexedBox{
		values: [&value]
	}
	mut copied := Box{
		value: &value
	}
	set_pair_sources(mut pair, &value)
	store_at_computed_index(mut indexed, &value)
	copy_then_store(mut copied, &value)
	set_or_reset(mut first, &value, &value, true)
	delegate_then_reset(mut second, &value, &value)
	store_in_loop(mut first, &value, &value, true)
	store_or_replace(mut second, &value, &value, true)
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut calls := map[string]flat.NodeId{}
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		for wanted in ['set_pair_sources', 'store_at_computed_index', 'copy_then_store',
			'set_or_reset', 'delegate_then_reset', 'store_in_loop', 'store_or_replace'] {
			if name.ends_with(wanted) {
				calls[wanted] = flat.NodeId(i)
			}
		}
	}
	assert calls.len == 7, calls.str()
	assert tc.call_param_storage_source_params(calls['set_pair_sources'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_at_computed_index'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['copy_then_store'], 0) == []
	assert tc.call_param_storage_source_params(calls['set_or_reset'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['delegate_then_reset'], 0) == [2]
	assert tc.call_param_storage_source_params(calls['store_in_loop'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['store_or_replace'], 0) == [1, 2]
}
