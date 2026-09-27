interface Named {
	name() string
}

struct Record {
	label string
}

fn (r &Record) name() string { return r.label }

fn recover_record(item Named) ?&Record {
	return if item is Record { item } else { none }
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
	}
}
