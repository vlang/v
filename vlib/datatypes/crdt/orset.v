module crdt

import rand

// ORSet represents an Observed-Removed Set (OR-Set / Add-Wins Set) in CRDT.
// Each addition generates a globally unique tag (ULID). A deletion of an element
// moves all tags currently observed for that element into a removal set.
// An element is present if there is at least one tag in the add-set that has not been removed.
// Concurrent add and remove operations result in the add winning (Add-Wins).
pub struct ORSet[T] {
mut:
	add_tags map[T]map[string]u8
	rm_tags  map[T]map[string]u8
}

// new_orset creates a new empty ORSet.
pub fn new_orset[T]() ORSet[T] {
	return ORSet[T]{
		add_tags: map[T]map[string]u8{}
		rm_tags:  map[T]map[string]u8{}
	}
}

// add inserts an element into the ORSet with a newly generated unique tag and returns that tag.
pub fn (mut o ORSet[T]) add(elem T) string {
	tag := rand.ulid().str()
	o.add_with_tag(elem, tag)
	return tag
}

// add_with_tag inserts an element into the ORSet with a specific unique tag.
pub fn (mut o ORSet[T]) add_with_tag(elem T, tag string) {
	if elem !in o.add_tags {
		o.add_tags[elem] = map[string]u8{}
	}
	o.add_tags[elem][tag] = 1
}

// remove marks all currently observed tags of the given element as removed.
pub fn (mut o ORSet[T]) remove(elem T) {
	if elem in o.add_tags {
		if elem !in o.rm_tags {
			o.rm_tags[elem] = map[string]u8{}
		}
		for tag, _ in o.add_tags[elem] {
			o.rm_tags[elem][tag] = 1
		}
	}
}

// lookup returns true if the element has at least one active tag that has not been removed.
pub fn (o &ORSet[T]) lookup(elem T) bool {
	if elem !in o.add_tags {
		return false
	}
	if elem !in o.rm_tags {
		return o.add_tags[elem].len > 0
	}
	for tag, _ in o.add_tags[elem] {
		if tag !in o.rm_tags[elem] {
			return true
		}
	}
	return false
}

// elements returns an array of all active elements currently belonging to the set.
pub fn (o &ORSet[T]) elements() []T {
	mut res := []T{}
	for elem, _ in o.add_tags {
		if o.lookup(elem) {
			res << elem
		}
	}
	return res
}

// len returns the count of active elements currently belonging to the set.
pub fn (o &ORSet[T]) len() int {
	return o.elements().len
}

// == checks whether two ORSets have identical add-tags and remove-tags.
pub fn (l ORSet[T]) == (r ORSet[T]) bool {
	if l.add_tags.len != r.add_tags.len || l.rm_tags.len != r.rm_tags.len {
		return false
	}
	for elem, tags in l.add_tags {
		if elem !in r.add_tags || tags.len != r.add_tags[elem].len {
			return false
		}
		for tag, _ in tags {
			if tag !in r.add_tags[elem] {
				return false
			}
		}
	}
	for elem, tags in l.rm_tags {
		if elem !in r.rm_tags || tags.len != r.rm_tags[elem].len {
			return false
		}
		for tag, _ in tags {
			if tag !in r.rm_tags[elem] {
				return false
			}
		}
	}
	return true
}

// merge combines state from another ORSet by taking the union of add-tags and remove-tags.
pub fn (mut o ORSet[T]) merge(other &ORSet[T]) {
	for elem, tags in other.add_tags {
		if elem !in o.add_tags {
			o.add_tags[elem] = tags.clone()
		} else {
			for tag, _ in tags {
				o.add_tags[elem][tag] = 1
			}
		}
	}
	for elem, tags in other.rm_tags {
		if elem !in o.rm_tags {
			o.rm_tags[elem] = tags.clone()
		} else {
			for tag, _ in tags {
				o.rm_tags[elem][tag] = 1
			}
		}
	}
}

