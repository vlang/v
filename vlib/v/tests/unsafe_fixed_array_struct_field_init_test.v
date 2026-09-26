// https://github.com/vlang/v/issues/28948
// An `unsafe { ... }` fixed-array value has to behave like the unwrapped one wherever
// it is copied into fixed-array storage: struct fields, assignments, args and returns.
interface UnsafeFixedElem {
	name() string
}

struct UnsafeFixedNamed {}

fn (n UnsafeFixedNamed) name() string {
	return 'named'
}

struct UnsafeFixedMaps {
mut:
	elems [1]map[string]int
}

struct UnsafeFixedIfaceMaps {
mut:
	elems [1]map[string][]UnsafeFixedElem
}

struct UnsafeFixedDefaultIfaceMaps {
mut:
	elems [1]map[string][]UnsafeFixedElem = unsafe { [1]map[string][]UnsafeFixedElem{} }
}

struct UnsafeFixedStrings {
	elems [2]string
}

struct UnsafeFixedArrays {
mut:
	elems [2][]int
}

struct UnsafeFixedItem {
mut:
	n int = 7
	m map[string]int
}

struct UnsafeFixedItems {
mut:
	elems [2]UnsafeFixedItem
}

struct UnsafeFixedInitMaps {
	elems [2]map[string]int
}

struct UnsafeFixedInts {
	elems [3]int
}

struct UnsafeFixedOuter {
mut:
	inner UnsafeFixedMaps
}

fn unsafe_fixed_maps_len(a [1]map[string]int) int {
	mut m := a[0].clone()
	m['x'] = 2
	return m['x'] + m.len + a[0].len
}

fn unsafe_fixed_maps() [1]map[string]int {
	return unsafe { [1]map[string]int{} }
}

fn test_unsafe_fixed_array_of_maps_struct_field() {
	mut foo := UnsafeFixedMaps{
		elems: unsafe { [1]map[string]int{} }
	}
	assert foo.elems.len == 1
	assert foo.elems[0].len == 0
	foo.elems[0]['x'] = 11
	assert foo.elems[0]['x'] == 11
	assert foo.elems[0].len == 1
}

fn test_unsafe_fixed_array_of_interface_maps_struct_field() {
	mut foo := UnsafeFixedIfaceMaps{
		elems: unsafe { [1]map[string][]UnsafeFixedElem{} }
	}
	foo.elems[0]['a'] = [UnsafeFixedElem(UnsafeFixedNamed{})]
	assert foo.elems[0]['a'][0].name() == 'named'
	mut dflt := UnsafeFixedDefaultIfaceMaps{}
	dflt.elems[0]['b'] = [UnsafeFixedElem(UnsafeFixedNamed{})]
	assert dflt.elems[0]['b'][0].name() == 'named'
}

fn test_unsafe_fixed_array_of_strings_and_arrays_struct_fields() {
	strs := UnsafeFixedStrings{
		elems: unsafe { [2]string{} }
	}
	assert strs.elems == ['', '']!
	mut arrs := UnsafeFixedArrays{
		elems: unsafe { [2][]int{} }
	}
	arrs.elems[1] << 5
	assert arrs.elems[0].len == 0
	assert arrs.elems[1] == [5]
}

fn test_unsafe_fixed_array_of_structs_with_defaults_struct_field() {
	mut items := UnsafeFixedItems{
		elems: unsafe { [2]UnsafeFixedItem{} }
	}
	assert items.elems[1].n == 7
	items.elems[1].m['k'] = 3
	assert items.elems[1].m['k'] == 3
}

fn test_unsafe_fixed_array_with_init_struct_field() {
	maps := UnsafeFixedInitMaps{
		elems: unsafe {
			[2]map[string]int{init: {
				'a': index
			}}
		}
	}
	assert maps.elems[0]['a'] == 0
	assert maps.elems[1]['a'] == 1
	ints := UnsafeFixedInts{
		elems: unsafe { [3]int{init: index * 2} }
	}
	assert ints.elems == [0, 2, 4]!
}

fn test_unsafe_fixed_array_literal_with_hoisted_values_struct_field() {
	m := {
		'q': 1
	}
	maps := UnsafeFixedInitMaps{
		elems: unsafe {
			[m, {
				'r': 2
			}]!
		}
	}
	assert maps.elems[0]['q'] == 1
	assert maps.elems[1]['r'] == 2
}

fn test_unsafe_fixed_array_nested_and_heap_struct_fields() {
	mut outer := UnsafeFixedOuter{
		inner: UnsafeFixedMaps{
			elems: unsafe { [1]map[string]int{} }
		}
	}
	outer.inner.elems[0]['x'] = 4
	assert outer.inner.elems[0]['x'] == 4
	mut heap := &UnsafeFixedMaps{
		elems: unsafe { [1]map[string]int{} }
	}
	heap.elems[0]['x'] = 12
	assert heap.elems[0]['x'] == 12
}

fn test_unsafe_fixed_array_of_maps_assignments() {
	mut foo := UnsafeFixedMaps{}
	foo.elems = unsafe { [1]map[string]int{} }
	foo.elems[0]['x'] = 9
	assert foo.elems[0]['x'] == 9
	mut local := [1]map[string]int{}
	local = unsafe { [1]map[string]int{} }
	local[0]['z'] = 1
	assert local[0]['z'] == 1
}

fn test_unsafe_fixed_array_of_maps_arg_and_return() {
	assert unsafe_fixed_maps_len(unsafe { [1]map[string]int{} }) == 3
	mut a := unsafe_fixed_maps()
	a[0]['y'] = 5
	assert a[0]['y'] == 5
}

fn unsafe_fixed_pick(sel [3]int, i int) UnsafeFixedInts {
	a := [1, 2, 3]!
	b := [4, 5, 6]!
	return UnsafeFixedInts{
		elems: unsafe {
			if sel[i] == 0 {
				a
			} else {
				b
			}
		}
	}
}

fn test_unsafe_fixed_array_if_value_struct_field() {
	assert unsafe_fixed_pick([0, 1, 0]!, 0).elems == [1, 2, 3]!
	assert unsafe_fixed_pick([0, 1, 0]!, 1).elems == [4, 5, 6]!
}
