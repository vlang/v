#include "@DIR/owning_slot_guard.h"

fn C.slot_guard_drop(code int)
fn C.slot_guard_count(code int) int

interface Drop {
mut:
	drop()
}

struct SlotItem implements Drop {
	code int
}

fn (mut item SlotItem) drop() { C.slot_guard_drop(item.code) }

fn (item &SlotItem) free() { C.slot_guard_drop(item.code) }

@[aligned: 64]
struct SlotWide {
	value int
}

type SlotChoice = SlotWide | &SlotItem
type PairPointer = &[2]SlotChoice
type SlotOwner = int | PairPointer

fn replace(mut value PairPointer, other PairPointer) { value = other }

fn rebind(mut value SlotOwner, other PairPointer) {
	// Replace the enclosing sum so the old owner is released and `other` stays borrowed.
	value = SlotOwner(other)
}

fn C.slot_guard_next() int

fn next_owned_slot_choice() SlotChoice { return SlotChoice(&SlotItem{ code: C.slot_guard_next() }) }

fn owned_rebind(other PairPointer) {
	mut owner := SlotOwner(PairPointer(&[2]SlotChoice{init: next_owned_slot_choice()}))
	assert C.slot_guard_count(1) == 0
	assert C.slot_guard_count(2) == 0
	rebind(mut owner, other)
	assert voidptr(owner as PairPointer) == voidptr(other)
	// Check release before owner cleanup can touch the borrowed replacement.
	assert C.slot_guard_count(1) == 1
	assert C.slot_guard_count(2) == 1
	assert C.slot_guard_count(3) == 0
}

fn main() {
	{
		mut borrowed := [SlotChoice(&SlotItem{ code: 3 }), SlotChoice(SlotWide{ value: 9 })]!
		other := PairPointer(unsafe { &borrowed })
		owned_rebind(other)
		assert C.slot_guard_count(3) == 0
		assert (borrowed[0] as &SlotItem).code == 3
	}
	assert C.slot_guard_count(1) == 1
	assert C.slot_guard_count(2) == 1
	assert C.slot_guard_count(3) == 1
}
