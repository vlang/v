from pathlib import Path


def replace_once(text: str, old: str, new: str, label: str) -> str:
    if new in text:
        return text
    if old not in text:
        raise SystemExit(f"missing patch anchor: {label}")
    return text.replace(old, new, 1)


array_path = Path('vlib/v3/transform/array.v')
array_text = array_path.read_text()
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
    "\t\t\t\tt.array_map_record_local_pointer_origins(path, t.a.child(&node, branch_idx),\n"
    "\t\t\t\t\telem_name, origins, mut locals)\n"
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
    "\t\t\tt.array_map_record_local_pointer_origins(path, t.a.child(&node, i), elem_name,\n"
    "\t\t\t\torigins, mut branch_origins)\n"
    "\t\t\tfor branch_path, external in branch_origins {\n"
    "\t\t\t\tlocals[branch_path] = locals[branch_path] || external\n"
    "\t\t\t}\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
array_text = replace_once(array_text, old_origins, new_origins, 'aggregate result origins')
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
    "\t\t\t\tt.array_map_record_local_pointer_pointees(path, t.a.child(&node, branch_idx),\n"
    "\t\t\t\t\torigins, mut locals)\n"
    "\t\t\t}\n"
    "\t\t\treturn\n"
    "\t\t}\n"
    "\t}\n"
    "\tif node.kind in [.if_expr, .match_stmt, .or_expr, .comptime_if] {\n"
    "\t\tbranch_start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }\n"
    "\t\tfor i in branch_start .. node.children_count {\n"
    "\t\t\tt.array_map_record_local_pointer_pointees(path, t.a.child(&node, i), origins, mut\n"
    "\t\t\t\tlocals)\n"
    "\t\t}\n"
    "\t\treturn\n"
    "\t}\n"
)
array_text = replace_once(array_text, old_pointees, new_pointees, 'aggregate pointee origins')
array_path.write_text(array_text)

checker_path = Path('vlib/v3/types/checker_tail.v')
checker_text = checker_path.read_text()
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
marker = '// Direct writes reached before any possible return replace the caller\'s prior source for the\n'
if 'fn (tc &TypeChecker) storage_write_can_be_bypassed_by_goto' not in checker_text:
    if marker not in checker_text:
        raise SystemExit('missing patch anchor: definite-write helper')
    checker_text = checker_text.replace(marker, helper + marker, 1)
old_alias_setup = (
    "\tif target_param_idx >= param_names.len {\n"
    "\t\treturn []string{}\n"
    "\t}\n"
    "\tmut aliases := map[string][]int{}\n"
)
new_alias_setup = (
    "\tif target_param_idx >= param_names.len {\n"
    "\t\treturn []string{}\n"
    "\t}\n"
    "\tmut gotos := []flat.NodeId{}\n"
    "\tmut labels := map[string][]flat.NodeId{}\n"
    "\tfor i in body_start .. fn_node.children_count {\n"
    "\t\ttc.collect_visible_binding_gotos_and_labels(tc.a.child(&fn_node, i), mut gotos, mut\n"
    "\t\t\tlabels)\n"
    "\t}\n"
    "\tmut aliases := map[string][]int{}\n"
)
checker_text = replace_once(checker_text, old_alias_setup, new_alias_setup, 'goto collection')
old_path_record = (
    "\t\t\t\tif path := tc.storage_lvalue_path_from_param(tc.a.child(&child, j),\n"
    "\t\t\t\t\ttarget_param.value, aliases, target_param_idx)\n"
    "\t\t\t\t{\n"
    "\t\t\t\t\tif path !in paths {\n"
    "\t\t\t\t\t\tpaths << path\n"
    "\t\t\t\t\t}\n"
    "\t\t\t\t}\n"
)
new_path_record = (
    "\t\t\t\tlhs_id := tc.a.child(&child, j)\n"
    "\t\t\t\tif !tc.storage_write_can_be_bypassed_by_goto(tc.a.node(lhs_id).pos, gotos,\n"
    "\t\t\t\t\tlabels) {\n"
    "\t\t\t\t\tif path := tc.storage_lvalue_path_from_param(lhs_id, target_param.value, aliases,\n"
    "\t\t\t\t\t\ttarget_param_idx)\n"
    "\t\t\t\t\t{\n"
    "\t\t\t\t\t\tif path !in paths {\n"
    "\t\t\t\t\t\t\tpaths << path\n"
    "\t\t\t\t\t\t}\n"
    "\t\t\t\t\t}\n"
    "\t\t\t\t}\n"
)
checker_text = replace_once(checker_text, old_path_record, new_path_record, 'goto-aware definite write')
checker_path.write_text(checker_text)

transform_test_path = Path('vlib/v3/tests/review_transform_regressions_test.v')
transform_test = transform_test_path.read_text()
transform_test_fn = r'''

fn test_array_map_tracks_aggregate_pointer_origins_from_or_and_comptime_results() {
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

fn fallback_holder() !Holder {
	return error("fallback")
}

fn main() {
	external := Item{
		text: "external"
	}
	mut or_saved := PointerBox{
		value: unsafe { &external }
	}
	mut comptime_saved := PointerBox{
		value: unsafe { &external }
	}
	_ := make_items().map(match true {
		true {
			or_holder := fallback_holder() or {
				Holder{
					box: &or_saved
				}
			}
			comptime_holder := $if true {
				Holder{
					box: &comptime_saved
				}
			} $else {
				Holder{
					box: &or_saved
				}
			}
			or_holder.box.value = unsafe { &it }
			comptime_holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(or_saved.value.text)
	println(comptime_saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_aggregate_join_origins_c',
		'-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_aggregate_join_origins', '-ownership',
		source)
	assert out == 'source\
source'
}
'''
if 'fn test_array_map_tracks_aggregate_pointer_origins_from_or_and_comptime_results()' not in transform_test:
    transform_test_path.write_text(transform_test.rstrip() + transform_test_fn + '\n')

storage_test_path = Path('vlib/v3/types/storage_source_test.v')
storage_test = storage_test_path.read_text()
storage_test_fn = r'''

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
'''
if 'fn test_param_storage_sources_do_not_treat_goto_bypassed_write_as_definite()' not in storage_test:
    storage_test_path.write_text(storage_test.rstrip() + storage_test_fn + '\n')

for changed in [array_path, checker_path, transform_test_path, storage_test_path]:
    if not changed.read_text().endswith('\n'):
        raise SystemExit(f'missing trailing newline: {changed}')
