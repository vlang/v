module crdt

fn test_orset_basic() {
	mut orset := new_orset[string]()
	assert orset.lookup('item') == false

	orset.add('item')
	assert orset.lookup('item') == true
	assert orset.len() == 1

	orset.remove('item')
	assert orset.lookup('item') == false
	assert orset.len() == 0

	// In OR-Set, re-adding after removal works because a new unique tag is generated
	orset.add('item')
	assert orset.lookup('item') == true
	assert orset.len() == 1
}

fn test_orset_add_wins_concurrent() {
	mut replica_a := new_orset[string]()
	mut replica_b := new_orset[string]()

	// Both replicas start with 'doc' added and synced
	tag1 := replica_a.add('doc')
	replica_b.add_with_tag('doc', tag1)

	assert replica_a.lookup('doc') == true
	assert replica_b.lookup('doc') == true

	// Replica B removes 'doc' (it observes and removes tag1)
	replica_b.remove('doc')
	assert replica_b.lookup('doc') == false

	// Concurrently, Replica A adds 'doc' again (generates tag2, which B has not seen)
	_ := replica_a.add('doc')
	assert replica_a.lookup('doc') == true

	// Merge B into A and A into B
	replica_a.merge(&replica_b)
	replica_b.merge(&replica_a)

	// Add wins! 'doc' should be present because tag2 was not observed by B's removal
	assert replica_a.lookup('doc') == true
	assert replica_b.lookup('doc') == true
	assert replica_a == replica_b
}

fn test_orset_merge_idempotent() {
	mut s1 := new_orset[int]()
	mut s2 := new_orset[int]()

	s1.add(1)
	s2.add(2)

	s1.merge(&s2)
	assert s1.lookup(1)
	assert s1.lookup(2)

	// Idempotency
	s1.merge(&s2)
	assert s1.lookup(1)
	assert s1.lookup(2)
}
