// vtest ownership
// vtest vflags: -ownership
module arc

struct WeakTestValue implements Drop {
	drops chan bool
}

fn (mut value WeakTestValue) drop() {
	value.drops <- true
}

fn test_weak_references_do_not_keep_values_alive() {
	drops := chan bool{cap: 2}
	weak := make_weak_reference(drops)
	assert drops.len == 1
	assert weak.upgrade() == none
	assert weak.clone().upgrade() == none
}

fn make_weak_reference(drops chan bool) Weak[WeakTestValue] {
	strong := new(WeakTestValue{ drops: drops })
	weak := strong.downgrade()
	copy := weak.clone()
	assert strong.strong_count() == 1
	if upgraded := copy.upgrade() {
		assert ptr_eq(&strong, &upgraded)
		assert upgraded.strong_count() == 2
	} else {
		assert false
	}
	assert strong.strong_count() == 1
	return weak
}

fn test_upgrade_can_outlive_original_arc() {
	drops := chan bool{cap: 2}
	mut strong := new(WeakTestValue{ drops: drops })
	weak := strong.downgrade()
	{
		upgraded := weak.upgrade() or { panic('expected a live allocation') }
		strong.drop()
		assert drops.len == 0
		assert upgraded.strong_count() == 1
	}
	assert drops.len == 1
	assert weak.upgrade() == none
}

fn test_empty_weak_reference() {
	weak := Weak[int]{}
	assert weak.upgrade() == none
	assert weak.clone().upgrade() == none
}

fn test_weak_upgrade_can_race_with_last_strong_drop() {
	mut strong := new(42)
	weak := strong.downgrade()
	start := chan bool{cap: 1}
	worker := spawn check_weak_upgrades(weak.clone(), start)
	assert <-start
	strong.drop()
	worker.wait()
	assert weak.upgrade() == none
}

fn check_weak_upgrades(weak Weak[int], start chan bool) {
	{
		strong := weak.upgrade() or { panic('expected a live allocation') }
		assert *strong.get() == 42
	}
	start <- true
	for _ in 0 .. 1000 {
		strong := weak.upgrade() or { return }
		assert *strong.get() == 42
	}
}
