// A local whose address escapes is moved to the heap. Locals are lexical bindings: a
// same-named local of a later sibling scope that is not moved (a pointer here) must not
// be lowered as the moved one, and `&x` of it must stay the address of the pointer.

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

fn set_out(p &&u64, v &u64) {
	unsafe {
		*p = v
	}
}

fn through_pointer(target &u64) u64 {
	mut x := unsafe { &u64(nil) }
	slot := &x
	unsafe {
		*slot = target
	}
	return *x
}

fn in_match_branches(mut values []&u64, kind int, target &u64) u64 {
	mut got := u64(0)
	match kind {
		0 {
			x := u64(7)
			values << &x
		}
		else {
			mut x := unsafe { &u64(nil) }
			slot := &x
			unsafe {
				*slot = target
			}
			set_out(&x, target)
			got = *x
		}
	}
	return got
}

fn in_loop_bodies(mut values []&u64, targets []&u64) u64 {
	mut got := u64(0)
	for i in 0 .. 1 {
		x := u64(i + 8)
		values << &x
	}
	for x in targets {
		got += *x
	}
	for _ in 0 .. 1 {
		mut x := unsafe { &u64(nil) }
		set_out(&x, targets[0])
		got += *x
	}
	return got
}

fn in_blocks(mut values []&u64, target &u64) u64 {
	mut got := u64(0)
	unsafe {
		x := u64(9)
		values << &x
	}
	unsafe {
		mut x := &u64(nil)
		slot := &x
		*slot = target
		got = *x
	}
	return got
}

fn in_value_branches(mut values []&u64, kind int, target &u64) u64 {
	a := if kind == 0 {
		x := u64(10)
		values << &x
		u64(0)
	} else {
		mut x := unsafe { &u64(nil) }
		set_out(&x, target)
		*x
	}
	b := match kind {
		0 {
			x := u64(11)
			values << &x
			u64(0)
		}
		else {
			mut x := unsafe { &u64(nil) }
			set_out(&x, target)
			*x
		}
	}
	return a + b
}

// The moved local itself stays moved after nested scopes in its own scope.
fn after_nested_scopes(mut values []&u64, data u64) u64 {
	x := data
	values << &x
	if data > 0 {
		y := data + 1
		values << &y
	}
	for _ in 0 .. 1 {
		z := data + 2
		values << &z
	}
	return x + 1
}

fn test_same_named_locals_of_sibling_scopes() {
	mut values := []&u64{}
	target := u64(99)
	assert through_pointer(&target) == 99
	assert in_match_branches(mut values, 0, &target) == 0
	assert in_match_branches(mut values, 1, &target) == 99
	assert in_loop_bodies(mut values, [&target]) == 198
	assert in_blocks(mut values, &target) == 99
	assert in_value_branches(mut values, 0, &target) == 0
	assert in_value_branches(mut values, 1, &target) == 198
	assert after_nested_scopes(mut values, 20) == 21
	_ = use_the_stack(10)
	assert values.map(*it) == [u64(7), 8, 9, 10, 11, 20, 21, 22]
}

// A local moved to the heap in a nested scope of a `for mut` loop does not disturb the
// loop variable, which is also read and written through a pointer.
fn test_mut_loop_variable_after_a_nested_scope() {
	mut numbers := [u64(1), 2]
	mut values := []&u64{}
	for mut n in numbers {
		unsafe {
			copy := *n + 10
			values << &copy
		}
		n++
	}
	_ = use_the_stack(10)
	assert numbers == [u64(2), 3]
	assert values.map(*it) == [u64(11), 12]
}

type HeapScopeAlias = int

fn retained_type_names(mut values []&int, mut aliases []&HeapScopeAlias) []string {
	x := 7
	values << &x
	y := HeapScopeAlias(8)
	aliases << &y
	return [typeof(x).name, typeof(y).name]
}

fn test_heap_promotion_preserves_semantic_type_reflection() {
	mut values := []&int{}
	mut aliases := []&HeapScopeAlias{}
	assert retained_type_names(mut values, mut aliases) == ['int', 'HeapScopeAlias']
	_ = use_the_stack(10)
	assert *values[0] == 7
	assert *aliases[0] == HeapScopeAlias(8)
}
