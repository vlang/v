module transform

import v.flat
import v.types

fn unchecked_box_scan_fixture() (&flat.FlatAst, &types.TypeChecker, []int) {
	mut a := flat.FlatAst.new()
	file := a.add_val(.file, 'dependency.v')
	mod := a.add_val(.module_decl, 'dependency')
	global_value := a.add_val(.int_literal, '1')
	global_decl := a.add_val(.global_decl, 'global_value')
	used_body := a.add_val(.int_literal, '2')
	used_fn := a.add_val(.fn_decl, 'used')
	cold_body := a.add_val(.int_literal, '3')
	cold_fn := a.add_val(.fn_decl, 'cold')
	generic_body := a.add_val(.int_literal, '4')
	mut generic_node := flat.Node{ kind: .fn_decl, value: 'generic' }
	generic_node.set_generic_params(['T'])
	generic_fn := a.add_node(generic_node)
	mut tc := types.TypeChecker.new(a)
	tc.top_level_idx = [int(file), int(mod), int(global_decl), int(used_fn), int(cold_fn),
		int(generic_fn)]
	tc.top_level_idx_nodes_len = a.nodes.len
	tc.file_modules['dependency.v'] = 'dependency'
	tc.library_files['dependency.v'] = true
	tc.skips_library_bodies = true
	tc.reachable_library_fns['dependency.used'] = true
	return a, tc, [int(global_value), int(global_decl), int(used_body), int(used_fn), int(cold_body),
		int(cold_fn), int(generic_body), int(generic_fn)]
}

fn test_unchecked_box_scan_keeps_declarations_checked_bodies_and_generic_templates() {
	mut a, tc, ids := unchecked_box_scan_fixture()
	mut t := new_transformer(mut a, tc, map[string]bool{})
	t.skip_generics = true
	mask := t.unchecked_library_box_nodes()
	assert mask.len == a.nodes.len
	for idx in ids[..4] {
		assert !mask[idx], idx.str()
	}
	for idx in ids[4..6] {
		assert mask[idx], idx.str()
	}
	for idx in ids[6..] {
		assert !mask[idx], idx.str()
	}
	t.interface_boxed_skip_nodes = mask
	fork_tc := tc.fork_for_parallel_transform(a)
	assert !fork_tc.skips_library_bodies
	scan := t.fork_scan_worker(fork_tc)
	assert scan.interface_boxed_skip_nodes == mask
}

fn test_unchecked_box_scan_falls_back_for_incomplete_or_reordered_declaration_indexes() {
	mut a, checker, _ := unchecked_box_scan_fixture()
	mut tc := checker
	mut t := new_transformer(mut a, tc, map[string]bool{})
	t.skip_generics = true
	tc.top_level_idx_nodes_len--
	assert t.unchecked_library_box_nodes().len == 0
	tc.top_level_idx_nodes_len = a.nodes.len
	tc.top_level_idx[2] = tc.top_level_idx[1]
	assert t.unchecked_library_box_nodes().len == 0
}

fn test_unchecked_box_scan_preserves_full_and_generic_builds_and_nil_checker() {
	mut a, checker, _ := unchecked_box_scan_fixture()
	mut tc := checker
	mut t := new_transformer(mut a, tc, map[string]bool{})
	assert t.unchecked_library_box_nodes().len == 0
	t.skip_generics = true
	t.building_v = true
	assert t.unchecked_library_box_nodes().len == 0
	t.building_v = false
	tc.skips_library_bodies = false
	assert t.unchecked_library_box_nodes().len == 0
	t.tc = unsafe { nil }
	assert t.unchecked_library_box_nodes().len == 0
}

fn test_unchecked_box_scan_keeps_reflected_method_implementations_and_autofree_metadata() {
	mut a, checker, _ := unchecked_box_scan_fixture()
	mut tc := checker
	mut t := new_transformer(mut a, tc, map[string]bool{})
	t.skip_generics = true
	reflection := a.add_val(.comptime_for, 'method|fields')
	tc.top_level_idx_nodes_len = a.nodes.len
	assert t.unchecked_library_box_nodes().len == a.nodes.len
	a.nodes[int(reflection)].value = 'method|methods'
	assert t.unchecked_library_box_nodes().len == 0
	a.nodes[int(reflection)].value = 'method|fields'
	tc.autofree_mode = true
	assert t.unchecked_library_box_nodes().len == 0
}
