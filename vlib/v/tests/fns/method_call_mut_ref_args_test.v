// Regression test for https://github.com/vlang/v/issues/28864 :
// a method with a `mut e &T` parameter must receive the address of the
// caller's reference, just like a plain function does.
struct Entry {
mut:
	n int
}

struct Holder {
mut:
	calls int
}

fn (h Holder) fill(mut e &Entry) {
	e.n = 42
}

fn (mut h Holder) fill_counted(mut e &Entry) {
	h.calls++
	e.n = 43
}

fn (h &Holder) fill_ref(mut e &Entry) {
	e.n = 44
}

fn (h Holder) replace(mut e &Entry) {
	e = &Entry{
		n: 7
	}
}

fn (h Holder) forward(mut e &Entry) {
	h.fill(mut e)
}

fn (h Holder) fill_generic[T](mut e &T) {
	e.n = 45
}

fn (h Holder) fill_two(mut a &Entry, mut b &Entry) {
	a.n = 1
	b = &Entry{
		n: 2
	}
}

struct GenericHolder[T] {
	x T
}

fn (h GenericHolder[T]) fill(mut e &Entry) {
	e.n = 46
}

struct Outer {
	Holder
}

struct Box {
mut:
	e &Entry
}

fn test_value_receiver() {
	h := Holder{}
	mut e := &Entry{}
	h.fill(mut e)
	assert e.n == 42
}

fn test_mut_receiver() {
	mut h := Holder{}
	mut e := &Entry{}
	h.fill_counted(mut e)
	assert e.n == 43
	assert h.calls == 1
}

fn test_pointer_receiver() {
	h := &Holder{}
	mut e := &Entry{}
	h.fill_ref(mut e)
	assert e.n == 44
}

fn test_reassign_reference_in_method() {
	h := Holder{}
	mut e := &Entry{}
	old := e
	h.replace(mut e)
	assert e.n == 7
	assert old.n == 0
	assert voidptr(e) != voidptr(old)
}

fn test_forward_mut_ref_param_to_method() {
	h := Holder{}
	mut e := &Entry{}
	h.forward(mut e)
	assert e.n == 42
}

fn test_generic_method() {
	h := Holder{}
	mut e := &Entry{}
	h.fill_generic(mut e)
	assert e.n == 45
}

fn test_generic_struct_method() {
	h := GenericHolder[int]{}
	mut e := &Entry{}
	h.fill(mut e)
	assert e.n == 46
}

fn test_multiple_mut_ref_args() {
	h := Holder{}
	mut a := &Entry{}
	mut b := &Entry{}
	old_b := b
	h.fill_two(mut a, mut b)
	assert a.n == 1
	assert b.n == 2
	assert voidptr(b) != voidptr(old_b)
}

fn test_indexed_receiver() {
	hs := [Holder{}]
	mut e := &Entry{}
	hs[0].fill(mut e)
	assert e.n == 42
}

fn test_embedded_method() {
	o := Outer{}
	mut e := &Entry{}
	o.fill(mut e)
	assert e.n == 42
}

fn test_field_and_index_args() {
	h := Holder{}
	mut b := Box{
		e: &Entry{}
	}
	h.fill(mut b.e)
	assert b.e.n == 42
	mut entries := [&Entry{}, &Entry{}]
	h.replace(mut entries[1])
	assert entries[0].n == 0
	assert entries[1].n == 7
}
