module flat

fn test_type_lists_keep_order_empty_entries_and_independent_clones() {
	mut bindings := TypeTextListBindings{}
	bindings.set('call', ['pkg.Alias', '', 'pkg.Alias'])
	bindings.set('empty', []string{})
	mut fork := bindings.clone()
	fork.append('call', ['&pkg.T'])
	assert bindings.value('call') == ['pkg.Alias', '', 'pkg.Alias']
	assert fork.value('call') == ['pkg.Alias', '', 'pkg.Alias', '&pkg.T']
	assert bindings.contains('empty')
	assert bindings.get('empty') or { panic('empty list was lost') } == []string{}
	assert !bindings.contains('absent')
}

fn test_setting_numeric_lists_copies_caller_storage() {
	mut ids := [source_type_id('pkg.T')]
	mut bindings := TypeTextListBindings{}
	bindings.set_ids('call', ids)
	ids[0] = source_type_id('pkg.Other')
	assert bindings.value('call') == ['pkg.T']
}

fn test_appending_numeric_lists_copies_values_and_preserves_empty_entries() {
	first := source_type_id('pkg.First')
	second := source_type_id('pkg.Second')
	mut ids := [second, u32(0), second]
	mut bindings := TypeTextListBindings{}
	bindings.set_ids('call', [first])
	bindings.append_ids('call', ids)
	ids[0] = first
	assert bindings.type_ids('call') or { panic('missing call') } == [first, second, 0, second]
	bindings.append_ids('absent', []u32{})
	assert !bindings.contains('absent')
}
