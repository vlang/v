@[has_globals]
module main

interface Counter {
mut:
	next() int
}

struct RealCounter {
mut:
	n int
}

fn (mut r RealCounter) next() int {
	r.n++
	return r.n
}

struct Holder {
pub mut:
	slot ?Counter
}

fn make_counter(ok bool) ?&RealCounter {
	if ok {
		return &RealCounter{}
	}
	return none
}

fn test_option_of_struct_ref_assigned_to_option_of_interface() {
	mut h := &Holder{}
	mut c := make_counter(true)
	h.slot = c
	if mut s := h.slot {
		assert s.next() == 1
		assert s.next() == 2
	} else {
		assert false, 'a successful concrete option became none'
	}
	backing := c or { panic('missing backing counter') }
	assert backing.n == 2
}

fn test_none_option_of_struct_ref_assigned_to_option_of_interface() {
	mut h := &Holder{}
	h.slot = make_counter(false)
	assert h.slot == none
}

interface NamedCounter {
	name string
	value() int
}

struct ConcreteNamedCounter {
	name string
	n    int
}

fn (c ConcreteNamedCounter) value() int {
	return c.n
}

struct NamedHolder {
mut:
	slot ?NamedCounter
}

fn named_counter_value() ?ConcreteNamedCounter {
	return ConcreteNamedCounter{ name: 'value', n: 41 }
}

fn make_named_holder() NamedHolder {
	mut holder := NamedHolder{}
	value := named_counter_value()
	holder.slot = value
	return holder
}

fn test_option_value_box_retains_cached_field_after_return() {
	holder := make_named_holder()
	slot := holder.slot or { panic('boxed value became none') }
	assert slot.name == 'value'
	assert slot.value() == 41
}

fn test_option_pointer_box_retains_cached_field_and_none() {
	mut holder := NamedHolder{}
	value := ?&ConcreteNamedCounter(&ConcreteNamedCounter{ name: 'pointer', n: 42 })
	holder.slot = value
	slot := holder.slot or { panic('boxed pointer became none') }
	assert slot.name == 'pointer'
	assert slot.value() == 42
	none_value := ?&ConcreteNamedCounter(none)
	holder.slot = none_value
	assert holder.slot == none
}

__global optional_counter_calls int

fn counted_counter(ok bool) ?&RealCounter {
	optional_counter_calls++
	if ok {
		return &RealCounter{ n: 40 }
	}
	return none
}

fn test_direct_option_call_is_evaluated_once_for_success_and_none() {
	optional_counter_calls = 0
	mut holder := Holder{}
	holder.slot = counted_counter(true)
	assert optional_counter_calls == 1
	if mut slot := holder.slot {
		assert slot.next() == 41
	} else {
		assert false, 'a direct successful call became none'
	}
	holder.slot = counted_counter(false)
	assert optional_counter_calls == 2
	assert holder.slot == none
}
