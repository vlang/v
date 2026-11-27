module sketches

fn test_hyperloglog_cardinality() {
	mut hll := new_hyperloglog(12, 1)!
	for i in 0 .. 5000 {
		hll.add('item${i}')
	}
	c := hll.count()
	assert c > 4500.0 && c < 5500.0
	mut h2 := new_hyperloglog(12, 1)!
	h2.add('extra')
	hll.merge(h2)!
	assert hll.count() >= c
}

fn test_countmin_no_underestimate() {
	mut cm := new_countmin(1024, 4, 7)!
	for _ in 0 .. 10 {
		cm.add('hot', 1)
	}
	assert cm.estimate('hot') >= 10
	assert cm.estimate('never-seen') == 0
}

fn test_countmin_saturates() {
	mut cm := new_countmin(8, 2, 7)!
	cm.add('hot', max_counter)
	cm.add('hot', max_counter)
	assert cm.estimate('hot') == max_counter
}

fn test_space_saving_topk() {
	mut ss := new_space_saving(3)!
	for _ in 0 .. 100 {
		ss.add('a', 1)
	}
	for _ in 0 .. 50 {
		ss.add('b', 1)
	}
	for i in 0 .. 30 {
		ss.add('c${i}', 1)
	}
	top := ss.top(2)
	assert top.len == 2
	assert top[0].key == 'a'
	assert top[1].key == 'b'
	assert ss.count('a') == 100
}
