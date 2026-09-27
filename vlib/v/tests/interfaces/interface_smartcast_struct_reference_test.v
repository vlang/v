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
	}
}
