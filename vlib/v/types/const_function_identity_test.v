module types

import os
import v.flat
import v.parser
import v.pref

fn test_const_cycle_check_still_checks_function_arguments() {
	path := os.join_path(os.vtmp_dir(), 'v3_const_function_cycle_${os.getpid()}.v')
	os.write_file(path, 'const answer = answer(answer)\nfn answer(x int) int { return x }\nfn main() {}\n')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg == 'cycle in constant `answer`'), tc.errors.str()
}

fn const_function_namespace_selector(mut a flat.FlatAst, namespace string, name string) flat.NodeId {
	base := a.add_val(.ident, namespace)
	start := a.begin_children()
	a.add_child(base)
	return a.add_node(flat.Node{
		kind:           .selector
		value:          name
		children_start: start
		children_count: 1
	})
}

fn test_namespace_constant_keeps_its_type_and_storage_identity() {
	for fast in [false, true] {
		for namespace in ['answer', 'renamed'] {
			for constant_type in [builtin_int_type, Type(FnType{ return_type: builtin_string_type })] {
				mut a := flat.FlatAst.new()
				selector := const_function_namespace_selector(mut a, namespace, 'value')
				mut tc := TypeChecker.new(&a)
				tc.cur_module = 'main'
				tc.cur_file = 'main.v'
				tc.valid_resolution_fast = fast
				tc.file_imports_by_file['main.v'] = &FileImportInfo{
					imports: {
						'answer':  'answer'
						'renamed': 'answer'
					}
				}
				tc.const_types['answer.value'] = constant_type
				tc.fn_param_types['answer.value'] = []Type{}
				tc.fn_ret_types['answer.value'] = builtin_int_type

				assert tc.resolve_type_uncached(selector).name() == constant_type.name()
				assert tc.infer_fn_value_decl_type(selector) == none
				tc.check_selector(selector, *a.node(selector))
				assert tc.errors.len == 0, tc.errors.str()
				assert tc.resolve_type(selector).name() == constant_type.name()
				assert tc.resolved_fn_value_name(selector) == none
			}
		}
	}
}

fn test_namespace_constant_precedence_preserves_direct_calls_and_function_values() {
	root := os.join_path(os.vtmp_dir(), 'v3_const_function_namespace_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dependency := os.join_path(root, 'answer.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dependency, 'module answer
pub const value = value()
pub const callback = label
pub fn value() int { return 42 }
pub fn only() int { return 43 }
pub fn label() string { return "constant callback" }
')!
	os.write_file(main, 'module main
import answer as renamed
fn consume(callback fn () int) int { return callback() }
fn main() {
	integer := renamed.value
	assert integer == 42
	assert renamed.value() == 42
	function := renamed.only
	assert function() == 43
	assert consume(renamed.only) == 43
	callback := renamed.callback
	assert callback() == "constant callback"
}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dependency, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut value_calls := 0
	mut constant_reads := 0
	mut function_values := 0
	for i, node in a.nodes {
		id := flat.NodeId(i)
		if node.kind == .call && tc.resolved_call_name(id) or { '' } == 'answer.value' {
			assert tc.resolve_type(id).name() == 'int'
			value_calls++
		}
		if node.kind != .selector || tc.ident_is_call_callee_or_generic_base(id) {
			continue
		}
		match node.value {
			'value', 'callback' {
				assert tc.resolved_fn_value_name(id) == none
				constant_type := tc.const_types['answer.${node.value}'] or {
					panic('missing constant type for answer.${node.value}')
				}
				assert tc.resolve_type(id).name() == constant_type.name()
				constant_reads++
			}
			'only' {
				assert tc.resolved_fn_value_name(id) or { '' } == 'answer.only'
				assert tc.resolve_type(id) is FnType
				function_values++
			}
			else {}
		}
	}
	assert value_calls == 2
	assert constant_reads == 2
	assert function_values == 2
}
