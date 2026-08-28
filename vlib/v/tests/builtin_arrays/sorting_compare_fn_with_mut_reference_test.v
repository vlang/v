struct Thing {
mut:
	a  int = 2
	b  int = 4
	av int
}

fn (mut t Thing) average() int {
	t.av = (t.a + t.b) / 2
	return t.av
}

struct Things {
mut:
	items []&Thing
}

fn (mut t Things) sort() {
	t.items.sort_with_compare(fn (mut a &Thing, mut b &Thing) int {
		if a.average() > b.average() {
			return 1
		} else if a.average() < b.average() {
			return -1
		}
		return 0
	})
}

fn test_sort_compare_fn_with_mut_ref_param() {
	mut t := Things{}
	t.items << &Thing{2, 4, 0}
	t.items << &Thing{5, 7, 0}
	t.items << &Thing{1, 2, 0}
	t.sort()
	println(t)
	assert t.items.len == 3
	assert t.items[0] == &Thing{1, 2, 1}
	assert t.items[1] == &Thing{2, 4, 3}
	assert t.items[2] == &Thing{5, 7, 6}
}

fn compare_thing_slots(a &&Thing, b &&Thing) int {
	return (*a).a - (*b).a
}

fn test_sort_compare_fn_with_pointer_slots() {
	mut items := [&Thing{a: 3}, &Thing{a: 1}, &Thing{a: 2}]
	items.sort_with_compare(compare_thing_slots)
	assert items.map(it.a) == [1, 2, 3]
}

fn compare_thing_values(a &Thing, b &Thing) int {
	return a.a - b.a
}

fn test_sort_compare_local_fn_with_pointer_values() {
	mut items := [&Thing{a: 3}, &Thing{a: 1}, &Thing{a: 2}]
	compare := compare_thing_values
	items.sort_with_compare(compare)
	assert items.map(it.a) == [1, 2, 3]
}

struct ThingComparators {
	@[required]
	compare fn (&Thing, &Thing) int
}

fn test_sort_compare_selector_fn_with_pointer_values() {
	mut items := [&Thing{a: 3}, &Thing{a: 1}, &Thing{a: 2}]
	comparators := ThingComparators{
		compare: compare_thing_values
	}
	items.sort_with_compare(comparators.compare)
	assert items.map(it.a) == [1, 2, 3]
}

type ThingCompare = fn (&Thing, &Thing) int

fn test_sort_compare_alias_fn_with_pointer_values() {
	mut items := [&Thing{a: 3}, &Thing{a: 1}, &Thing{a: 2}]
	compare := ThingCompare(compare_thing_values)
	items.sort_with_compare(compare)
	assert items.map(it.a) == [1, 2, 3]
}
