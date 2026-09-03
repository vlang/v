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

struct BoxHolder {
	mut:
	box &Box
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

fn store_through_aggregate(mut box &Box, value &Value) {
	tmp := Box{
		value: unsafe { value }
	}
	box.value = tmp.value
}

fn store_through_target_aggregate(mut box Box, value &Value) {
	mut holder := BoxHolder{
		box: unsafe { &box }
	}
	holder.box.value = value
}

fn store_deferred(mut box &Box, value &Value, replacement &Value) {
	defer {
		box.value = value
	}
	box.value = replacement
}

fn store_selected(mut box &Box, value &Value, replacement &Value, signal chan bool) {
	select {
		<-signal {
			box.value = value
		}
		else {
			box.value = replacement
		}
	}
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

fn reset_then_delegate(mut box &Box, value &Value, replacement &Value) {
	box.value = value
	store_value(mut box, replacement)
}

fn keep_value(mut box &Box) {
	box.value = box.value
}

fn set_then_keep(mut box &Box, value &Value) {
	box.value = value
	keep_value(mut box)
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
	store_through_aggregate(mut first, &value)
	store_through_target_aggregate(mut copied, &value)
	store_deferred(mut first, &value, &value)
	store_selected(mut first, &value, &value, chan bool{})
	set_or_reset(mut first, &value, &value, true)
	delegate_then_reset(mut second, &value, &value)
	reset_then_delegate(mut second, &value, &value)
	set_then_keep(mut second, &value)
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
			'store_through_aggregate', 'store_through_target_aggregate', 'store_deferred',
			'store_selected', 'set_or_reset', 'delegate_then_reset', 'reset_then_delegate',
			'set_then_keep', 'store_in_loop', 'store_or_replace'] {
			if name.ends_with(wanted) {
				calls[wanted] = flat.NodeId(i)
			}
		}
	}
	assert calls.len == 13, calls.str()
	assert tc.call_param_storage_source_params(calls['set_pair_sources'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_at_computed_index'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['copy_then_store'], 0) == []
	assert tc.call_param_storage_source_params(calls['store_through_aggregate'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['store_through_target_aggregate'], 0) == [
		1,
	]
	assert tc.call_param_storage_source_params(calls['store_deferred'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_selected'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['set_or_reset'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['delegate_then_reset'], 0) == [2]
	assert tc.call_param_storage_source_params(calls['reset_then_delegate'], 0) == [2]
	assert tc.call_param_storage_source_params(calls['set_then_keep'], 0) == [1]
	assert tc.call_param_storage_source_params(calls['store_in_loop'], 0) == [1, 2]
	assert tc.call_param_storage_source_params(calls['store_or_replace'], 0) == [1, 2]
}

fn test_param_storage_sources_use_call_site_module_for_unqualified_calls() {
	dir := os.join_path(os.vtmp_dir(), 'v3_storage_source_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	other_path := os.join_path(dir, 'other.v')
	main_path := os.join_path(dir, 'main.v')
	os.write_file(other_path, 'module other

struct OtherBox {}

fn replace(mut target &OtherBox, replacement &OtherBox) {
	_ = target
	_ = replacement
}
') or { panic(err) }
	os.write_file(main_path, 'struct Box {}

fn replace(mut target &Box, replacement &Box) bool {
	target = replacement
	return true
}

fn main() {
	mut first := &Box{}
	second := &Box{}
	changed := replace(mut first, second)
	assert changed
}
') or { panic(err) }

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([other_path, main_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut replace_call := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('replace') {
			replace_call = flat.NodeId(i)
		}
	}
	assert int(replace_call) >= 0
	assert tc.call_param_storage_source_params(replace_call, 0) == [1]
}

fn test_param_storage_sources_snapshot_aliases_before_multi_assignment() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_multi_assign_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn swap_aliases_then_store(mut target &Box, value &Value) {
	mut local_value := Value{}
	mut local := &Box{
		value: &local_value
	}
	mut alias := target
	mut other := local
	alias, other = other, alias
	other.value = value
}

fn main() {
	mut value := Value{}
	mut target := &Box{
		value: &value
	}
	swap_aliases_then_store(mut target, &value)
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

	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('swap_aliases_then_store') {
			call_id = flat.NodeId(i)
		}
	}
	assert int(call_id) >= 0
	assert tc.call_param_storage_source_params(call_id, 0) == [1]
}

fn test_param_storage_sources_follow_c_for_execution_order() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_c_for_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn store_in_c_for(mut target &Box, value &Value, replacement &Value) {
	mut local := &Box{
		value: unsafe { replacement }
	}
	mut alias := local
	mut i := 0
	for alias = target; i < 1; alias = local {
		alias.value = value
		i++
	}
}

fn main() {
	mut value := Value{}
	mut target := &Box{
		value: &value
	}
	store_in_c_for(mut target, &value, &value)
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

	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('store_in_c_for') {
			call_id = flat.NodeId(i)
		}
	}
	assert int(call_id) >= 0
	sources := tc.call_param_storage_source_params(call_id, 0)
	assert 1 in sources, sources.str()
}

fn test_param_storage_sources_do_not_treat_goto_bypassed_write_as_definite() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_goto_definite_${os.getpid()}.v')
	os.write_file(path, 'struct Value {}

struct Box {
mut:
	value &Value
}

fn maybe_replace(mut target &Box, replacement &Value, skip bool) {
	if skip {
		unsafe { goto done }
	}
	target.value = replacement
done:
}

fn wrapper(mut target &Box, first &Value, replacement &Value, skip bool) {
	target.value = first
	maybe_replace(mut target, replacement, skip)
}

fn main() {
	mut first := Value{}
	mut replacement := Value{}
	mut target := &Box{
		value: &first
	}
	wrapper(mut target, &first, &replacement, true)
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

	mut wrapper_call := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('wrapper') {
			wrapper_call = flat.NodeId(i)
		}
	}
	assert int(wrapper_call) >= 0
	assert tc.call_param_storage_source_params(wrapper_call, 0) == [1, 2]
}
