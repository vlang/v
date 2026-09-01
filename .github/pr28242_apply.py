from pathlib import Path


def replace_once(path: str, old: str, new: str) -> None:
    file = Path(path)
    text = file.read_text()
    count = text.count(old)
    if count != 1:
        raise SystemExit(f'{path}: expected one replacement, found {count}')
    file.write_text(text.replace(old, new, 1))


array_path = 'vlib/v3/transform/array.v'
old_origins = (
    "\tif node.kind in [.if_expr, .match_stmt] {\n"
    "\t\tlocals[path] = false\n"
    "\t\tfor i in 1 .. node.children_count {\n"
    "\t\t\tmut branch_origins := map[string]bool{}\n"
    "\t\t\tt.array_map_record_local_pointer_origins(path, t.a.child(&node, i), elem_name, origins, mut branch_origins)\n"
    "\t\t\tfor branch_path, external in branch_origins {\n"
    "\t\t\t\tlocals[branch_path] = locals[branch_path] || external\n"
    "\t\t\t}\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
new_origins = (
    "\tif node.kind == .comptime_if {\n"
    "\t\tif take_then := t.comptime_type_condition_value(node.value) {\n"
    "\t\t\tbranch_idx := if take_then { 0 } else { 1 }\n"
    "\t\t\tif branch_idx < node.children_count {\n"
    "\t\t\t\tt.array_map_record_local_pointer_origins(path, t.a.child(&node, branch_idx), elem_name, origins, mut locals)\n"
    "\t\t\t} else {\n"
    "\t\t\t\tlocals[path] = false\n"
    "\t\t\t}\n"
    "\t\t\treturn\n"
    "\t\t}\n"
    "\t}\n"
    "\tif node.kind in [.if_expr, .match_stmt, .or_expr, .comptime_if] {\n"
    "\t\tlocals[path] = false\n"
    "\t\tbranch_start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }\n"
    "\t\tfor i in branch_start .. node.children_count {\n"
    "\t\t\tmut branch_origins := map[string]bool{}\n"
    "\t\t\tt.array_map_record_local_pointer_origins(path, t.a.child(&node, i), elem_name, origins, mut branch_origins)\n"
    "\t\t\tfor branch_path, external in branch_origins {\n"
    "\t\t\t\tlocals[branch_path] = locals[branch_path] || external\n"
    "\t\t\t}\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
replace_once(array_path, old_origins, new_origins)

old_pointees = (
    "\tif node.kind in [.if_expr, .match_stmt] {\n"
    "\t\tfor i in 1 .. node.children_count {\n"
    "\t\t\tt.array_map_record_local_pointer_pointees(path, t.a.child(&node, i), origins, mut locals)\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
new_pointees = (
    "\tif node.kind == .comptime_if {\n"
    "\t\tif take_then := t.comptime_type_condition_value(node.value) {\n"
    "\t\t\tbranch_idx := if take_then { 0 } else { 1 }\n"
    "\t\t\tif branch_idx < node.children_count {\n"
    "\t\t\t\tt.array_map_record_local_pointer_pointees(path, t.a.child(&node, branch_idx), origins, mut locals)\n"
    "\t\t\t}\n"
    "\t\t\treturn\n"
    "\t\t}\n"
    "\t}\n"
    "\tif node.kind in [.if_expr, .match_stmt, .or_expr, .comptime_if] {\n"
    "\t\tbranch_start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }\n"
    "\t\tfor i in branch_start .. node.children_count {\n"
    "\t\t\tt.array_map_record_local_pointer_pointees(path, t.a.child(&node, i), origins, mut locals)\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
replace_once(array_path, old_pointees, new_pointees)

checker_path = 'vlib/v3/types/checker_tail.v'
marker = (
    "// Direct writes reached before any possible return replace the caller's prior source for the\n"
    "// same exact path. Conditional and delegated writes remain conservative unions.\n"
)
helper = (
    "fn (tc &TypeChecker) storage_write_can_be_bypassed_by_goto(pos token.Pos, gotos []flat.NodeId, labels map[string][]flat.NodeId) bool {\n"
    "\tfor goto_id in gotos {\n"
    "\t\tjump := tc.a.node(goto_id)\n"
    "\t\tif jump.pos.id != pos.id || jump.pos.offset >= pos.offset {\n"
    "\t\t\tcontinue\n"
    "\t\t}\n"
    "\t\tfor label_id in labels[jump.value] {\n"
    "\t\t\tlabel := tc.a.node(label_id)\n"
    "\t\t\tif label.pos.id == pos.id && label.pos.offset > pos.offset {\n"
    "\t\t\t\treturn true\n"
    "\t\t\t}\n"
    "\t\t}\n"
    "\t}\n"
    "\treturn false\n"
    "}\n\n"
)
replace_once(checker_path, marker, helper + marker)

old_alias_setup = (
    "\tmut aliases := map[string][]int{}\n"
    "\tfor i, name in param_names {\n"
    "\t\taliases[name] = [i]\n"
    "\t}\n"
    "\tmut paths := []string{}\n"
)
new_alias_setup = (
    "\tmut aliases := map[string][]int{}\n"
    "\tfor i, name in param_names {\n"
    "\t\taliases[name] = [i]\n"
    "\t}\n"
    "\tmut gotos := []flat.NodeId{}\n"
    "\tmut labels := map[string][]flat.NodeId{}\n"
    "\tfor i in body_start .. fn_node.children_count {\n"
    "\t\ttc.collect_visible_binding_gotos_and_labels(tc.a.child(&fn_node, i), mut gotos, mut labels)\n"
    "\t}\n"
    "\tmut paths := []string{}\n"
)
replace_once(checker_path, old_alias_setup, new_alias_setup)

old_write = (
    "\t\t\t\tif path := tc.storage_lvalue_path_from_param(tc.a.child(&child, j),\n"
    "\t\t\t\t\ttarget_param.value, aliases, target_param_idx)\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\tif path !in paths {\n"
    "\t\t\t\t\t\tpaths << path\n"
    "\t\t\t\t\t}\n"
    "\t\t\t\t}\n"
)
new_write = (
    "\t\t\t\tlhs_id := tc.a.child(&child, j)\n"
    "\t\t\t\tif tc.storage_write_can_be_bypassed_by_goto(tc.a.node(lhs_id).pos, gotos, labels) {\n"
    "\t\t\t\t\tcontinue\n"
    "\t\t\t\t}\n"
    "\t\t\t\tif path := tc.storage_lvalue_path_from_param(lhs_id, target_param.value, aliases,\n"
    "\t\t\t\t\ttarget_param_idx)\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\tif path !in paths {\n"
    "\t\t\t\t\t\tpaths << path\n"
    "\t\t\t\t\t}\n"
    "\t\t\t\t}\n"
)
replace_once(checker_path, old_write, new_write)

transform_test_path = Path('vlib/v3/tests/review_transform_regressions_test.v')
transform_test = r'''

fn test_array_map_tracks_aggregate_pointer_origins_through_or_and_comptime_results() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
	box &PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn maybe_holder() !Holder {
	return error("fallback")
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved_or := PointerBox{
		value: unsafe { &external }
	}
	_ := make_items().map(match true {
		true {
			mut holder := maybe_holder() or {
				Holder{
					box: &saved_or
				}
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	mut saved_comptime := PointerBox{
		value: unsafe { &external }
	}
	_ := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := $if true {
				Holder{
					box: &saved_comptime
				}
			} $else {
				Holder{
					box: &local
				}
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(saved_or.value.text)
	println(saved_comptime.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_aggregate_alt_result_origins_c',
		'-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_aggregate_alt_result_origins', '-ownership',
		source)
	assert out == 'source\
source'
}
'''
transform_text = transform_test_path.read_text()
transform_marker = 'fn test_array_map_tracks_aggregate_pointer_origins_through_or_and_comptime_results()'
if transform_marker in transform_text:
    raise SystemExit('transform regression already exists')
transform_test_path.write_text(transform_text.rstrip() + transform_test + '\n')

storage_test_path = Path('vlib/v3/types/storage_source_test.v')
storage_test = r'''

fn test_param_storage_sources_keep_values_when_goto_bypasses_nested_write() {
	path := os.join_path(os.vtmp_dir(), 'v3_storage_source_goto_${os.getpid()}.v')
	os.write_file(path, 'struct Item {}

struct Box {
mut:
	value &Item
}

fn maybe_replace(mut target &Box, replacement &Item, skip bool) {
	if skip {
		unsafe {
			goto done
		}
	}
	target.value = replacement
done:
}

fn wrapper(mut target &Box, first &Item, replacement &Item, skip bool) {
	target.value = first
	maybe_replace(mut target, replacement, skip)
}

fn main() {
	mut first := Item{}
	mut replacement := Item{}
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

	mut call_id := flat.empty_node
	for i, node in a.nodes {
		if node.kind != .call {
			continue
		}
		name := tc.resolved_call_name(flat.NodeId(i)) or { continue }
		if name.ends_with('wrapper') {
			call_id = flat.NodeId(i)
		}
	}
	assert int(call_id) >= 0
	assert tc.call_param_storage_source_params(call_id, 0) == [1, 2]
}
'''
storage_text = storage_test_path.read_text()
storage_marker = 'fn test_param_storage_sources_keep_values_when_goto_bypasses_nested_write()'
if storage_marker in storage_text:
    raise SystemExit('storage regression already exists')
storage_test_path.write_text(storage_text.rstrip() + storage_test + '\n')
