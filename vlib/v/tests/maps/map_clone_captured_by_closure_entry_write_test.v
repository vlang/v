// Entries of a cloned map may be replaced even when its values hold pointers
// shared with the original and an anonymous fn captures the clone.
struct SharedValue {
	id int
}

struct Entry {
	value &SharedValue = unsafe { nil }
}

type EntryMap = map[string]Entry

fn visit_entries(entries EntryMap, cb fn (string, Entry)) {
	for name, entry in entries {
		cb(name, entry)
	}
}

fn merged(own EntryMap, inherited EntryMap, use_callback bool) EntryMap {
	mut copy := own.clone()
	if use_callback {
		visit_entries(inherited, fn [mut copy] (name string, entry Entry) {
			if name !in copy {
				copy[name] = entry
			}
		})
		return copy
	}
	for name, entry in inherited {
		if name !in copy {
			copy[name] = entry
		}
	}
	return copy
}

fn test_entry_writes_to_cloned_map_captured_by_closure() {
	shared_value := &SharedValue{
		id: 7
	}
	mut own := EntryMap(map[string]Entry{})
	own['a'] = Entry{
		value: shared_value
	}
	inherited := EntryMap({
		'b': Entry{
			value: shared_value
		}
	})
	result := merged(own, inherited, false)
	assert result.len == 2
	assert result['b'].value.id == 7
	assert own.len == 1
	assert merged(own, inherited, true).len >= 1
}
