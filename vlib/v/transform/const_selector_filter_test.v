module transform

import v.flat
import v.types

fn test_const_selector_filter_preserves_qualified_suffix_import_and_builtin_visibility() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for name in ['main_only', 'shared', 'builtin.shared', 'alpha.value', 'nested.mod.limit',
		'beta.value', 'module', 'éclair', '@escaped', '_private', 'odd..ending', 'trailing.'] {
		tc.const_types[name] = types.Type(types.int_)
	}
	tc.const_modules['main_only'] = 'main'
	tc.const_modules['shared'] = 'main'
	tc.file_imports[file_import_key('one.v', 'dep')] = 'nested.mod'
	tc.file_imports[file_import_key('two.v', 'dep')] = 'alpha'
	mut slow := new_transformer(mut a, &tc, map[string]bool{})
	mut fast := new_transformer(mut a, &tc, map[string]bool{})
	slow.collect_const_suffixes()
	fast.collect_const_suffixes()
	// Suffix keys are a separate authority; custom spellings need not have the
	// same final segment as the constant key they resolve to.
	slow.const_suffixes['alternate'] = 'nested.mod.limit'
	fast.const_suffixes['alternate'] = 'nested.mod.limit'
	fast.prepare_const_selector_filter()
	names := ['main_only', 'shared', 'builtin.shared', 'alpha.value', 'nested.mod.limit', 'beta.value',
		'value', 'mod.limit', 'limit', 'dep.limit', 'dep.value', 'alternate', 'module', 'mXdule',
		'éclair', '@escaped', '_private', 'odd..ending', 'trailing.', 'missing', 'missing.field',
		'']
	for name in names {
		assert fast.const_type_key(name) == slow.const_type_key(name)
		for module_name in ['', 'main', 'builtin', 'alpha', 'other'] {
			for file in ['one.v', 'two.v', 'missing.v'] {
				assert fast.const_type_key_in_context(name, module_name, file) == slow.const_type_key_in_context(name, module_name, file)
			}
		}
	}
	assert fast.const_type_key_in_context('dep.limit', 'main', 'one.v') or { '' } == 'nested.mod.limit'
	assert fast.const_type_key_in_context('shared', 'alpha', 'one.v') or { '' } == 'builtin.shared'
	assert fast.const_type_key_in_context('main_only', 'alpha', 'one.v') == none
	assert fast.const_type_key('alternate') or { '' } == 'nested.mod.limit'
	assert fast.const_selector_may_match('mXdule')
	assert !fast.const_selector_may_match('absent_selector_field')
}

fn test_const_selector_filter_preserves_raw_expression_and_local_shadowing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['answer'] = types.Type(types.int_)
	tc.const_types['config.value'] = types.Type(types.string_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_const_suffixes()
	answer := a.add_val(.ident, 'answer')
	base := a.add_val(.ident, 'config')
	start := a.children.len
	a.children << base
	selector := a.add_node(flat.Node{ kind: .selector, value: 'value', children_start: start, children_count: 1 })
	assert t.raw_const_type_name_for_expr(answer) or { u32(0) } == flat.source_type_int
	assert t.raw_const_type_name_for_expr(selector) or { u32(0) } == flat.source_type_string
	t.prepare_const_selector_filter()
	assert t.raw_const_type_name_for_expr(answer) or { u32(0) } == flat.source_type_int
	assert t.raw_const_type_name_for_expr(selector) or { u32(0) } == flat.source_type_string
	t.set_var_type('answer', flat.source_type_bool)
	assert t.raw_const_type_name_for_expr(answer) == none
	t.set_var_type('config', flat.source_type_id('Config'))
	assert t.raw_const_type_name_for_expr(selector) == none
	missing := a.add_val(.ident, 'absent_selector_field')
	assert t.raw_const_type_name_for_expr(missing) == none
}

fn test_const_selector_filter_falls_back_when_either_authority_grows_and_rebuilds() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.const_selector_may_match('later')
	t.prepare_const_selector_filter()
	assert !t.const_selector_may_match('later')
	tc.const_types['later'] = types.Type(types.int_)
	assert t.const_type_key('later') or { '' } == 'later'
	t.prepare_const_selector_filter()
	assert !t.const_selector_may_match('alternate')
	t.const_suffixes['alternate'] = 'later'
	assert t.const_type_key('alternate') or { '' } == 'later'
	t.prepare_const_selector_filter()
	assert t.const_selector_may_match('alternate')
	tc.const_types.delete('later')
	t.const_suffixes = map[string]string{}
	t.prepare_const_selector_filter()
	assert !t.const_selector_may_match('later')
	assert !t.const_selector_may_match('alternate')
}

fn test_const_selector_filter_forks_share_bits_and_mutable_query_buffers_are_read_again() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['open'] = types.Type(types.int_)
	tc.const_types['shut'] = types.Type(types.int_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_const_selector_filter()
	worker := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	scan := t.fork_scan_worker(&tc)
	assert voidptr(worker.const_selector_filter) == voidptr(t.const_selector_filter)
	assert voidptr(scan.const_selector_filter) == voidptr(t.const_selector_filter)
	mut bytes := 'open'.bytes()
	name := unsafe { tos(bytes.data, bytes.len) }
	for instance in [&t, &worker, scan] {
		assert instance.const_type_key(name) or { '' } == 'open'
	}
	for i, ch in 'shut'.bytes() { bytes[i] = ch }
	for instance in [&t, &worker, scan] {
		assert instance.const_type_key(name) or { '' } == 'shut'
	}
	for i, ch in 'zzzz'.bytes() { bytes[i] = ch }
	for instance in [&t, &worker, scan] {
		assert instance.const_type_key(name) == none
	}
}

fn test_const_selector_filter_reads_same_key_value_updates_without_rebuilding() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.const_types['answer'] = types.Type(types.int_)
	tc.const_types['label'] = types.Type(types.string_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_const_suffixes()
	t.const_suffixes['alternate'] = 'answer'
	t.prepare_const_selector_filter()
	worker := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	scan := t.fork_scan_worker(&tc)
	const_count := tc.const_types.len
	suffix_count := t.const_suffixes.len
	for instance in [&t, &worker, scan] {
		assert instance.const_type_name('answer') or { u32(0) } == flat.source_type_int
		assert instance.const_type_name('alternate') or { u32(0) } == flat.source_type_int
	}
	// Membership is frozen; both authorities still read the current values.
	tc.const_types['answer'] = types.Type(types.bool_)
	t.const_suffixes['alternate'] = 'label'
	t.const_suffixes['answer'] = 'label'
	assert tc.const_types.len == const_count
	assert t.const_suffixes.len == suffix_count
	for instance in [&t, &worker, scan] {
		assert instance.const_type_name('answer') or { u32(0) } == flat.source_type_bool
		assert instance.const_type_name('alternate') or { u32(0) } == flat.source_type_string
		assert instance.const_type_key('answer') or { '' } == 'answer'
		assert instance.const_type_key_in_context('answer', 'main', '') or { '' } == 'answer'
	}
}
