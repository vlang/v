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
	if mut value is PairPointer {
		// Both pointers are borrowed; this deliberately exercises the unsafe slot ABI.
		slot := unsafe { &value }
		replace(mut *slot, other)
	} else {
		assert false
	}
}

fn borrowed_rebind(first PairPointer, other PairPointer) {
	mut owner := SlotOwner(first)
	rebind(mut owner, other)
	assert voidptr(owner as PairPointer) == voidptr(other)
	assert C.slot_guard_count(3) == 0
	assert C.slot_guard_count(4) == 0
}

fn main() {
	{
		mut first := [SlotChoice(&SlotItem{ code: 4 }), SlotChoice(SlotWide{ value: 7 })]!
		mut second := [SlotChoice(&SlotItem{ code: 3 }), SlotChoice(SlotWide{ value: 9 })]!
		first_ptr := PairPointer(unsafe { &first })
		second_ptr := PairPointer(unsafe { &second })
		borrowed_rebind(first_ptr, second_ptr)
		assert C.slot_guard_count(3) == 0
		assert C.slot_guard_count(4) == 0
		assert (first[0] as &SlotItem).code == 4
		assert (second[0] as &SlotItem).code == 3
	}
	assert C.slot_guard_count(3) == 1
	assert C.slot_guard_count(4) == 1
}
