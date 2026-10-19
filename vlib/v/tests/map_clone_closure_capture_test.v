// A closure that captures a cloned map (`fn [mut copy] ...`) does not make
// the clone alias the immutable source, so entries of the clone can still be
// replaced outside the closure.
struct MapCloneClosureValue {
	n int
}

struct MapCloneClosureProperty {
	id    int
	value &MapCloneClosureValue = unsafe { nil }
}

type MapCloneClosureMap = map[string]MapCloneClosureProperty

struct MapCloneClosureData {
	own   MapCloneClosureMap
	count int
}

fn (data &MapCloneClosureData) each(cb fn (string, MapCloneClosureProperty)) {
	for name, property in data.own {
		cb(name, property)
	}
}

fn map_clone_closure_merge(own MapCloneClosureMap, parent &MapCloneClosureData) MapCloneClosureMap {
	mut own_copy := own.clone()
	if parent.count > 5 {
		parent.each(fn [mut own_copy] (name string, property MapCloneClosureProperty) {
			if name !in own_copy {
				own_copy[name] = property
			}
		})
		return own_copy
	}
	for name, property in parent.own {
		if name !in own_copy {
			own_copy[name] = property
		}
	}
	return own_copy
}

fn test_map_clone_captured_by_closure_accepts_entry_writes() {
	value := &MapCloneClosureValue{
		n: 5
	}
	parent := &MapCloneClosureData{
		own: {
			'a': MapCloneClosureProperty{1, value}
		}
	}
	own := MapCloneClosureMap({
		'b': MapCloneClosureProperty{2, value}
	})
	merged := map_clone_closure_merge(own, parent)
	assert merged.len == 2
	assert merged['a'].id == 1
	assert merged['b'].value.n == 5
	assert own.len == 1
	assert 'a' !in own
}
