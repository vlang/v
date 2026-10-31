// A dynamic array of concrete values in a multi-return slot is converted element by
// element where an array of their interface or sum type is expected: in a forwarded
// `return f()` and in a `a, b = f()` assignment, as in a single return or assignment.
interface Named {
	name() string
}

interface Labeled {
	Named
	label() string
}

struct Item {
	n int
}

fn (item Item) name() string {
	return 'item${item.n}'
}

fn (item Item) label() string {
	return 'label${item.n}'
}

type Shape = Item | string

struct Holder {
mut:
	named []Named
}

fn make_items() (int, []Item) {
	return 7, [Item{1}, Item{2}]
}

fn make_fixed_items() (int, [2]Item) {
	return 7, [Item{3}, Item{4}]!
}

fn make_items_result() !(int, []Item) {
	return 7, [Item{5}, Item{6}]
}

fn make_items_option() ?(int, []Item) {
	return 7, [Item{7}, Item{8}]
}

fn make_labeled() (int, []Labeled, map[string]Labeled) {
	return 7, [Labeled(Item{9})], {
		'a': Labeled(Item{10})
	}
}

fn forward_named() (int, []Named) {
	return make_items()
}

fn forward_shapes() (int, []Shape) {
	return make_items()
}

fn forward_fixed_named() (int, []Named) {
	return make_fixed_items()
}

fn forward_named_result() !(int, []Named) {
	return make_items_result()
}

fn forward_named_option() ?(int, []Named) {
	return make_items_option()
}

fn forward_labeled_as_named() (int, []Named, map[string]Named) {
	return make_labeled()
}

fn names(values []Named) []string {
	return values.map(it.name())
}

fn test_forwarded_slots_are_converted() {
	n, named := forward_named()
	assert n == 7
	assert names(named) == ['item1', 'item2']
	_, shapes := forward_shapes()
	assert shapes.len == 2
	assert shapes[1] is Item
	assert (shapes[1] as Item).n == 2
	_, fixed := forward_fixed_named()
	assert names(fixed) == ['item3', 'item4']
	_, from_result := forward_named_result() or { panic(err) }
	assert names(from_result) == ['item5', 'item6']
	_, from_option := forward_named_option() or { panic('none') }
	assert names(from_option) == ['item7', 'item8']
}

fn test_forwarded_interface_container_slots_keep_their_values() {
	// An interface value is stored as another interface as it is, so a map of them
	// is not converted; the array is converted element by element.
	n, named, by_key := forward_labeled_as_named()
	assert n == 7
	assert names(named) == ['item9']
	value := by_key['a'] or { panic('missing') }
	assert value.name() == 'item10'
}

fn assign_to_mut_param(mut named []Named) int {
	mut n := 0
	n, named = make_items()
	return n
}

fn test_multi_return_assignment_slots_are_converted() {
	mut n := 0
	mut named := []Named{}
	n, named = make_items()
	assert n == 7
	assert names(named) == ['item1', 'item2']
	n, named = make_fixed_items()
	assert names(named) == ['item3', 'item4']
	n, named = make_items_result() or { panic(err) }
	assert names(named) == ['item5', 'item6']
	mut shapes := []Shape{}
	n, shapes = make_items()
	assert shapes.len == 2
	assert shapes[0] is Item
	assert (shapes[0] as Item).n == 1
	mut holder := Holder{}
	n, holder.named = make_items()
	assert names(holder.named) == ['item1', 'item2']
	mut param_target := []Named{}
	assert assign_to_mut_param(mut param_target) == 7
	assert names(param_target) == ['item1', 'item2']
}
