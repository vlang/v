module markused

import v.flat
import v.types

fn initializer_suffix_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn test_initializer_ref_complete_suffixes_keep_bare_candidates_and_order() {
	collector := CallCollector{
		const_decls:             {
			'dep.answer': ConstDeclInfo{}
			'answer':     ConstDeclInfo{}
			'dep.other':  ConstDeclInfo{}
		}
		const_suffixes:          {
			'answer': true
			'other':  true
		}
		const_suffixes_complete: true
	}
	mut refs := []string{}
	collector.add_initializer_ref_candidates('absent', 'dep', map[string]string{}, mut refs)
	assert refs.len == 0
	collector.add_initializer_ref_candidates('answer', 'dep', map[string]string{}, mut refs)
	assert refs == ['dep.answer', 'answer']
	collector.add_initializer_ref_candidates('other', 'dep', map[string]string{}, mut refs)
	collector.add_initializer_ref_candidates('answer', 'dep', map[string]string{}, mut refs)
	assert refs == ['dep.answer', 'answer', 'dep.other']
}

fn test_initializer_ref_suffixes_keep_qualified_import_and_main_candidates() {
	for complete in [false, true] {
		collector := CallCollector{
			const_decls:             {
				'consumer.alias.answer': ConstDeclInfo{}
				'alias.answer':          ConstDeclInfo{}
				'dep.answer':            ConstDeclInfo{}
				'main.answer':           ConstDeclInfo{}
				'answer':                ConstDeclInfo{}
			}
			const_suffixes:          {
				'answer': true
			}
			const_suffixes_complete: complete
		}
		mut imported := []string{}
		collector.add_initializer_ref_candidates('alias.answer', 'consumer', {
			'alias': 'dep'
		}, mut imported)
		assert imported == ['consumer.alias.answer', 'alias.answer', 'dep.answer']
		mut main_refs := []string{}
		collector.add_initializer_ref_candidates('main.answer', 'main', map[string]string{}, mut main_refs)
		assert main_refs == ['main.answer', 'answer']
		mut builtin_refs := []string{}
		collector.add_initializer_ref_candidates('answer', 'builtin', map[string]string{}, mut builtin_refs)
		assert builtin_refs == ['answer']
	}
}

fn test_initializer_ref_partial_manual_suffixes_retain_fallback_in_worker_view() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	for suffixes in [map[string]bool{}, {
		'other': true
	}] {
		collector := CallCollector{
			a:              &a
			tc:             &tc
			const_decls:    {
				'dep.answer': ConstDeclInfo{}
				'answer':     ConstDeclInfo{}
			}
			const_suffixes: suffixes
		}
		worker_tc := types.TypeChecker.new(&a)
		worker := collector.fork_with_tc(&worker_tc)
		mut refs := []string{}
		worker.add_initializer_ref_candidates('answer', 'dep', map[string]string{}, mut refs)
		assert refs == ['dep.answer', 'answer']
		assert collector.const_suffixes == suffixes
		mut original_refs := []string{}
		collector.add_initializer_ref_candidates('answer', 'dep', map[string]string{}, mut original_refs)
		assert original_refs == refs
	}
}

fn test_initializer_ref_suffix_gate_preserves_scoped_constant_shadowing() {
	mut a := flat.FlatAst.new()
	before := a.add_val(.ident, 'answer')
	lhs := a.add_val(.ident, 'answer')
	rhs := a.add_val(.int_literal, '7')
	decl := initializer_suffix_node(mut a, flat.Node{ kind: .decl_assign }, [lhs, rhs])
	inner := a.add_val(.ident, 'answer')
	block := initializer_suffix_node(mut a, flat.Node{ kind: .block }, [decl, inner])
	after := a.add_val(.ident, 'answer')
	body := initializer_suffix_node(mut a, flat.Node{ kind: .block }, [before, block, after])
	fn_id := initializer_suffix_node(mut a, flat.Node{ kind: .fn_decl, value: 'consume' }, [body])
	shadow_body := initializer_suffix_node(mut a, flat.Node{ kind: .block }, [block])
	shadow_fn_id := initializer_suffix_node(mut a, flat.Node{ kind: .fn_decl, value: 'shadow_only' }, [shadow_body])
	tc := types.TypeChecker.new(&a)
	for complete in [false, true] {
		collector := CallCollector{
			a:                       &a
			tc:                      &tc
			const_decls:             {
				'dep.answer': ConstDeclInfo{ expr_id: rhs }
			}
			const_suffixes:          {
				'answer': true
			}
			const_suffixes_complete: complete
			import_contexts:         [map[string]string{}]
		}
		result := collector.collect_body(a.node(fn_id), 'dep', map[string]string{})
		assert result.refs == ['dep.answer']
		assert result.calls.len == 0
		shadowed := collector.collect_body(a.node(shadow_fn_id), 'dep', map[string]string{})
		assert shadowed.refs.len == 0
		assert shadowed.calls.len == 0
	}
}

fn test_initializer_ref_empty_declarations_and_names_preserve_existing_refs() {
	for collector in [CallCollector{}, CallCollector{
		const_decls:             {
			'answer': ConstDeclInfo{}
		}
		const_suffixes_complete: true
	}] {
		mut refs := ['retained']
		collector.add_initializer_ref_candidates('', 'dep', map[string]string{}, mut refs)
		assert refs == ['retained']
	}
	collector := CallCollector{ const_suffixes_complete: true }
	mut refs := ['retained']
	collector.add_initializer_ref_candidates('answer', 'dep', map[string]string{}, mut refs)
	collector.add_initializer_ref_candidates('alias.answer', 'dep', {
		'alias': 'other'
	}, mut refs)
	assert refs == ['retained']
}
