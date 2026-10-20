#include "@DIR/aligned_owner.h"

fn C.aligned_owner_next() int
fn C.aligned_owner_visits() int
fn C.aligned_owner_record(code int)
fn C.aligned_owner_count(code int) int

interface Drop {
mut:
	drop()
}

struct AlignDropItem implements Drop {
	code int
}

fn (item &AlignDropItem) free() { C.aligned_owner_record(item.code) }

fn (mut item AlignDropItem) drop() { C.aligned_owner_record(item.code) }

@[aligned: 64]
struct AlignWide {
	value int
}

type AlignChoice = AlignWide | &AlignDropItem
type AlignArrayOwner = int | &[2]AlignChoice

fn next_choice() AlignChoice {
	return AlignChoice(&AlignDropItem{ code: C.aligned_owner_next() })
}

fn owned_array_scope() {
	owner := AlignArrayOwner(&[2]AlignChoice{init: next_choice()})
	values := owner as &[2]AlignChoice
	assert usize(voidptr(values)) % 64 == 0
	assert C.aligned_owner_visits() == 2
	assert ((*values)[0] as &AlignDropItem).code == 1
	assert ((*values)[1] as &AlignDropItem).code == 2
	assert C.aligned_owner_count(1) == 0
	assert C.aligned_owner_count(2) == 0
}

fn borrowed_array(values &[2]AlignChoice) &[2]AlignChoice {
	return values
}

fn borrowed_array_scope(values &[2]AlignChoice) {
	owner := AlignArrayOwner(borrowed_array(values))
	view := owner as &[2]AlignChoice
	assert voidptr(view) == voidptr(values)
	assert ((*view)[0] as &AlignDropItem).code == 3
}

fn borrowed_array_control() {
	mut values := [AlignChoice(&AlignDropItem{ code: 3 }), AlignChoice(AlignWide{ value: 9 })]!
	// The array stays live through every use of this borrowed reference.
	pointer := unsafe { &values }
	borrowed_array_scope(pointer)
	assert C.aligned_owner_count(3) == 0
	assert (values[0] as &AlignDropItem).code == 3
	// The returned pointer must keep referring to the caller's existing array.
	unsafe { (*pointer)[1] = AlignChoice(AlignWide{ value: 11 }) }
	assert (values[1] as AlignWide).value == 11
}

fn main() {
	owned_array_scope()
	borrowed_array_control()
	assert C.aligned_owner_count(1) == 1
	assert C.aligned_owner_count(2) == 1
	assert C.aligned_owner_count(3) == 1
	assert C.aligned_owner_visits() == 2
}
