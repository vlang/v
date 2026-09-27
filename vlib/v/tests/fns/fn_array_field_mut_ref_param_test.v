struct Handlers {
mut:
	list  []fn (mut &Handlers, int)
	named map[string]fn (mut &Handlers, int)
	total int
}

fn add(mut h &Handlers, n int) {
	h.total += n
}

fn add_twice(mut h &Handlers, n int) {
	h.total += n * 2
}

fn (mut h Handlers) run(i int, n int) {
	h.list[i](mut h, n)
}

fn test_indexed_fn_field_call_with_mut_ref_param() {
	mut h := Handlers{}
	h.list << add
	h.list << add_twice
	i := 1
	h.list[0](mut h, 1)
	h.list[i](mut h, 10)
	assert h.total == 21
	h.named['add'] = add
	h.named['add'](mut h, 100)
	assert h.total == 121
}

fn test_indexed_fn_field_call_from_mut_receiver() {
	mut h := Handlers{}
	h.list << add
	h.run(0, 5)
	assert h.total == 5
}

fn test_indexed_fn_field_call_with_pointer_arg() {
	mut h := &Handlers{}
	h.list << add
	h.list[0](mut h, 7)
	assert h.total == 7
}

fn test_indexed_local_fn_array_call_with_mut_ref_param() {
	mut h := Handlers{}
	fns := [add, add_twice]
	fns[1](mut h, 3)
	assert h.total == 6
}
