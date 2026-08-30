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

fn route_targets(mut first &Box, mut second &Box, value &Value) {
	_ = first
	second.value = value
}

fn set_pair_sources(mut pair Pair, value &Value) {
	route_targets(mut pair.first, mut pair.second, value)
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
	set_pair_sources(mut pair, &value)
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

	mut set_call := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('set_pair_sources') {
			set_call = flat.NodeId(i)
		}
	}
	assert set_call != flat.empty_node
	assert tc.call_param_storage_source_params(set_call, 0) == [1]
}
