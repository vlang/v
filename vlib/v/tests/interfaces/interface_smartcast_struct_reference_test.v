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
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
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
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
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
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('non reference type'), result.output
	}
}
