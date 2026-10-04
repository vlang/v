import os

interface Named {
	name() string
}

struct Record {
	label string
}

type RecordAlias = Record

fn (r &Record) name() string { return r.label }

fn recover_record(item Named) ?&Record {
	return if item is Record { item } else { none }
}

fn recover_record_alias(item Named) ?&RecordAlias {
	return if item is RecordAlias { item } else { none }
}

fn recover_record_match(item Named) ?&Record {
	match item {
		Record { return item }
		else { return none }
	}
}

fn recover_record_match_expression(item Named) ?&Record {
	return match item {
		Record { item }
		else { none }
	}
}

fn copy_record_match(item Named) Record {
	match item {
		Record { return *item }
		else { return Record{} }
	}
}

fn value_record_match(item Named) Record {
	match item {
		Record { return item }
		else { return Record{} }
	}
}

fn value_record_if(item Named) Record {
	if item is Record {
		return item
	}
	return Record{}
}

fn value_record_match_expression(item Named) Record {
	return match item {
		Record { item }
		else { Record{} }
	}
}

fn value_record_if_expression(item Named) Record {
	return if item is Record { item } else { Record{} }
}

fn optional_value_record_if(item Named) ?Record {
	return if item is Record { item } else { none }
}

fn optional_value_record_match(item Named) ?Record {
	return match item {
		Record { item }
		else { none }
	}
}

fn result_value_record_if(item Named) !Record {
	return if item is Record { item } else { error('missing record') }
}

fn result_value_record_match(item Named) !Record {
	return match item {
		Record { item }
		else { error('missing record') }
	}
}

struct OtherRecord {}

fn (_ OtherRecord) name() string {
	return 'other'
}

fn test_interface_struct_smartcast_copies_wrapped_values() {
	for item in [Named(&Record{ label: 'pointer' }), Named(Record{ label: 'value' })] {
		if item is Record {
			assert item == item
		}
		assert optional_value_record_if(item)?.label == item.name()
		assert optional_value_record_match(item)?.label == item.name()
		assert result_value_record_if(item)!.label == item.name()
		assert result_value_record_match(item)!.label == item.name()
	}
	other := Named(OtherRecord{})
	assert optional_value_record_if(other) == none
	assert optional_value_record_match(other) == none
	if _ := result_value_record_if(other) {
		assert false
	} else {
		assert err.msg() == 'missing record'
	}
	if _ := result_value_record_match(other) {
		assert false
	} else {
		assert err.msg() == 'missing record'
	}
	alias_item := Named(RecordAlias(Record{ label: 'alias' }))
	assert recover_record_alias(alias_item)?.label == 'alias'
}

fn test_interface_struct_smartcast_retains_object_reference() {
	for item in [Named(&Record{ label: 'pointer' }), Named(Record{ label: 'value' })] {
		if item is Record {
			copy := *item
			assert copy.label == item.label
		} else {
			assert false
		}
		recovered := recover_record(item) or { panic('missing record') }
		assert recovered.name() == item.name()
		matched := recover_record_match(item) or { panic('missing matched record') }
		assert matched == recovered
		matched_expression := recover_record_match_expression(item) or { panic('missing match result') }
		assert matched_expression == recovered
		assert copy_record_match(item).label == recovered.label
		assert value_record_match(item).label == recovered.label
		assert value_record_if(item).label == recovered.label
		assert value_record_match_expression(item).label == recovered.label
		assert value_record_if_expression(item).label == recovered.label
	}
}

fn test_explicit_pointer_branches_still_require_dereferencing_for_value_returns() {
	path := os.join_path(os.vtmp_dir(), 'interface_reference_return_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for expression in [
		'if flag { &Item{} } else { &Item{} }',
		'match flag { true { &Item{} } else { &Item{} } }',
	] {
		os.write_file(path, 'struct Item {}\nfn invalid(flag bool) Item { return ${expression} }\nfn main() { _ = invalid(true) }\n')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('non reference type'), result.output
	}
}

fn test_invalid_explicit_pointer_branches_in_wrapped_returns_stay_rejected() {
	path := os.join_path(os.vtmp_dir(), 'interface_wrapped_reference_return_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for body in [
		'?Item { return match flag { true { &Item{} } else { none } } }',
		"!Item { return if flag { &Item{} } else { error('missing') } }",
		"!Item { return match flag { true { &Item{} } else { error('missing') } } }",
	] {
		os.write_file(path, 'struct Item {}\nfn invalid(flag bool) ${body}\nfn main() {}\n')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('&Item'), result.output
	}
}

fn test_explicit_interface_pointer_smartcast_requires_dereference() {
	path := os.join_path(os.vtmp_dir(), 'interface_explicit_pointer_return_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for body in [
		'if item is &Record { return item } return Record{}',
		'return if item is &Record { item } else { Record{} }',
	] {
		os.write_file(path, 'interface Named { name() string }\nstruct Record {}\nfn (_ &Record) name() string { return "record" }\nfn invalid(item Named) Record { ${body} }\nfn main() {}\n')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('non reference type'), result.output
	}
}

fn test_explicit_pointer_patterns_require_dereferencing_for_value_returns() {
	path := os.join_path(os.vtmp_dir(), 'interface_pointer_pattern_return_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	preamble := 'interface Named { name() string }\nstruct Record { label string }\nfn (r &Record) name() string { return r.label }\nstruct Holder { item Named }\n'
	for body in [
		'Record { if item is PATTERN { return VALUE }; return Record{} }',
		'Record { return if item is PATTERN { VALUE } else { Record{} } }',
		'?Record { if item is PATTERN { return VALUE }; return none }',
		"!Record { if item is PATTERN { return VALUE }; return error('missing') }",
		'?Record { return if item is PATTERN { VALUE } else { none } }',
		"!Record { return if item is PATTERN { VALUE } else { error('missing') } }",
		'Record { if item is PATTERN { return match flag { true { VALUE } else { Record{} } } }; return Record{} }',
		'?Record { if item is PATTERN { return match flag { true { VALUE } else { none } } }; return none }',
		"!Record { if item is PATTERN { return match flag { true { VALUE } else { error('missing') } } }; return error('missing') }",
		'Record { assert item is PATTERN; return VALUE }',
		'Record { for flag { assert item is PATTERN; return VALUE }; return Record{} }',
		'Record { for _ in [flag] { assert item is PATTERN; return VALUE }; return Record{} }',
		'Record { select { else { assert item is PATTERN; return VALUE } }; return Record{} }',
		'Record { if item !is PATTERN { return Record{} }; return VALUE }',
		'Record { if item !is PATTERN { return Record{} } else { return VALUE } }',
		'Record { for item is PATTERN { return VALUE }; return Record{} }',
		'Record { if item is PATTERN && flag { return VALUE }; return Record{} }',
		'Record { is_record := item is PATTERN; if is_record { return VALUE }; return Record{} }',
		'Record { if item !is PATTERN || !flag { return Record{} }; return VALUE }',
		'Record { if item is PATTERN { if flag { return VALUE } }; return Record{} }',
		'Record { if item is PATTERN { reader := fn [item] () Record { return VALUE }; return reader() }; return Record{} }',
		'Record { assert item is PATTERN; reader := fn [item] () Record { return VALUE }; return reader() }',
		'Record { is_record := item is PATTERN; if is_record { reader := fn [item] () Record { return VALUE }; return reader() }; return Record{} }',
		'Record { if item !is PATTERN { return Record{} }; reader := fn [item] () Record { return VALUE }; return reader() }',
		'Record { if item is PATTERN { reader := fn [item] () Record { nested := fn [item] () Record { return VALUE }; return nested() }; return reader() }; return Record{} }',
	] {
		for mode in 0 .. 3 {
			pattern := if mode == 2 { 'Record' } else { '&Record' }
			value := if mode == 1 { '*item' } else { 'item' }
			declaration := body.replace('PATTERN', pattern).replace('VALUE', value)
			os.write_file(path, '${preamble}\nfn value(item Named, flag bool) ${declaration}\nfn main() {}\n')!
			result := os.exec([@VEXE, '-check', path])
			if mode == 0 {
				assert result.exit_code != 0, declaration
				assert result.output.contains('&Record')
					&& (result.output.contains('non reference type')
						|| result.output.contains('return type mismatch')
						|| result.output.contains('cannot use')), result.output
			} else {
				assert result.exit_code == 0, '${declaration}\n${result.output}'
			}
		}
	}
	for receiver in ['holder.item', 'items[0]'] {
		capture := receiver.all_before('.').all_before('[')
		for tail in [
			'if ${receiver} is PATTERN { return VALUE }; return Record{}',
			'if ${receiver} is PATTERN { reader := fn [${capture}] () Record { return VALUE }; return reader() }; return Record{}',
		] {
			for mode in 0 .. 3 {
				pattern := if mode == 2 { 'Record' } else { '&Record' }
				value := if mode == 1 { '*${receiver}' } else { receiver }
				body := 'holder := Holder{item}\nitems := [item]\n${tail.replace('PATTERN', pattern).replace('VALUE', value)}'
				os.write_file(path, '${preamble}\nfn value(item Named) Record { ${body} }\nfn main() {}\n')!
				result := os.exec([@VEXE, '-check', path])
				assert (result.exit_code == 0) == (mode != 0), '${body}\n${result.output}'
			}
		}
	}
}
