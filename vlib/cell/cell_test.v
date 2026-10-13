module cell

fn test_refcell_with_reads_value() {
	mut c := new_ref_cell(41)
	got := c.with(fn (v int) int {
		return v + 1
	})
	assert got == 42
	assert c.try_borrow() or { -1 } == 41, 'with must not modify the value'
}

fn test_refcell_with_mut_updates_value() {
	mut c := new_ref_cell(41)
	c.with_mut(fn (v int) int {
		return v + 1
	})
	assert c.try_borrow() or { -1 } == 42
}

fn test_refcell_nested_shared_borrows_coexist() {
	mut c := new_ref_cell(10)
	outer := c.with(fn [mut c] (v int) int {
		inner := c.with(fn (w int) int {
			return w * 2
		})
		return v + inner
	})
	assert outer == 30, 'shared borrows must nest'
}

fn test_refcell_try_with_mut_success() {
	mut c := new_ref_cell(10)
	// exclusive borrow held: shared access is refused ...
	held := c.try_with_mut(fn (v int) int {
		return v
	})
	assert held == true
	// ... but released afterwards, so access works again.
	assert c.try_borrow() or { -1 } == 10
}

fn test_refcell_try_with_none_while_exclusive() {
	mut c := new_ref_cell(10)
	ok := c.try_with_mut(fn [mut c] (v int) int {
		blocked := c.try_with(fn (w int) int {
			return w
		}) or { -1 }
		// shared access inside an exclusive borrow is refused ...
		assert blocked == -1, 'shared access during exclusive borrow must fail'
		// ... and an exclusive re-borrow is refused too.
		assert c.try_with_mut(fn (w int) int {
			return w
		}) == false, 'exclusive re-borrow must fail'
		return v
	})
	assert ok == true
	assert c.try_borrow() or { -1 } == 10
}
