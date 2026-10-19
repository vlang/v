struct FixedRef {
	p &[2]int
}

fn keep_fixed_ref(p &[2]int) FixedRef {
	return FixedRef{
		p: p
	}
}

fn insert_from_mut_fixed_param(mut a []int, mut f [2]int) {
	a.insert(1, f)
	a.prepend(f)
}

// Inserting or prepending a fixed-array local copies its elements, also when the local
// has been moved to the heap.
fn test_insert_and_prepend_a_fixed_array_local() {
	mut a := [3, 4]
	fixed := [7, 8]!
	a.insert(a.len, fixed)
	a.prepend(fixed)
	assert a == [7, 8, 3, 4, 7, 8]
	mut words := ['c']
	word_pair := ['a', 'b']!
	words.insert(1, word_pair)
	words.prepend(word_pair)
	assert words == ['a', 'b', 'c', 'a', 'b']
}

fn test_insert_a_fixed_array_local_whose_address_escapes() {
	mut a := [3, 4]
	fixed := [7, 8]!
	kept := keep_fixed_ref(&fixed)
	a.insert(0, fixed)
	a.prepend(fixed)
	assert a == [7, 8, 7, 8, 3, 4]
	assert unsafe { (*kept.p)[1] } == 8
}

fn test_insert_a_mut_fixed_array_param() {
	mut a := [0]
	mut f := [1, 2]!
	insert_from_mut_fixed_param(mut a, mut f)
	assert a == [1, 2, 0, 1, 2]
}

fn test_insert_a_fixed_array_captured_by_a_mut_closure() {
	mut captured := [5, 6]!
	set_first := fn [mut captured] () {
		captured[0] = 50
	}
	set_first()
	mut a := []int{}
	a.insert(0, captured)
	assert a == [50, 6]
}
