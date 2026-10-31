// V3 accepts an alias of a pointer to an array (V1 rejects it: arrays are already
// references). Appending the address of a local through it moves the local to the heap.

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

type AddressList = &[]&u64

type OtherAddressList = AddressList

struct AddressLists {
mut:
	direct  AddressList
	chained OtherAddressList
}

fn append_through_aliases(mut lists AddressLists, data int) {
	direct_source := u64(data)
	chained_source := u64(data) + 1
	lists.direct << &direct_source
	lists.chained << &chained_source
}

// The array can be reached through an alias of a pointer to it, or an alias of that.
fn test_address_appended_through_an_alias_of_a_pointer_to_an_array() {
	mut values := []&u64{}
	mut lists := AddressLists{
		direct:  &values
		chained: &values
	}
	append_through_aliases(mut lists, 40)
	_ = use_the_stack(10)
	assert values.len == 2
	assert *values[0] == 40
	assert *values[1] == 41
}
