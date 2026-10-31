module crdt

import time

fn test_lwweset_basic() {
	mut s := new_lwweset[string]()
	assert s.lookup('item') == false

	s.add('item')
	assert s.lookup('item') == true

	time.sleep(1 * time.millisecond)
	s.remove('item')
	assert s.lookup('item') == false

	time.sleep(1 * time.millisecond)
	// Unlike 2P-Set, re-add succeeds in LWW-Element-Set if timestamp is newer
	s.add('item')
	assert s.lookup('item') == true
}

fn test_lwweset_explicit_timestamps_and_bias() {
	mut s_add_wins := new_lwweset_with_bias[string](.add_wins)
	s_add_wins.add_with_time('k', 100)
	s_add_wins.remove_with_time('k', 100)
	// Equal timestamps: add_wins bias means element is present
	assert s_add_wins.lookup('k') == true

	mut s_rm_wins := new_lwweset_with_bias[string](.remove_wins)
	s_rm_wins.add_with_time('k', 100)
	s_rm_wins.remove_with_time('k', 100)
	// Equal timestamps: remove_wins bias means element is absent
	assert s_rm_wins.lookup('k') == false
}

fn test_lwweset_merge() {
	mut s1 := new_lwweset[string]()
	mut s2 := new_lwweset[string]()

	s1.add_with_time('apple', 10)
	s1.remove_with_time('banana', 20)

	s2.remove_with_time('apple', 15)
	s2.add_with_time('banana', 25)

	// Merge s2 into s1
	s1.merge(&s2)

	// apple was removed at t=15 > t=10
	assert s1.lookup('apple') == false

	// banana was added at t=25 > t=20
	assert s1.lookup('banana') == true

	// Commutativity
	s2.merge(&s1)
	assert s1 == s2
}
