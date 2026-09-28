#include <stdlib.h>

fn C.malloc(size usize) voidptr
fn C.free(value voidptr)

@[aligned: 8]
struct AllocatorAlignedCell {
	value int
}

type AllocatorAlignedCells = [2]AllocatorAlignedCell

fn release_c_aligned_cells(value &AllocatorAlignedCells) {
	unsafe { C.free(value) }
}

fn release_v_aligned_cells(value &AllocatorAlignedCells) {
	unsafe { free(value) }
}

fn test_aligned_array_cast_keeps_the_c_allocation_family() {
	// The requested alignment is within malloc's guarantee on the supported targets.
	mut cells := unsafe { &AllocatorAlignedCells(C.malloc(sizeof(AllocatorAlignedCells))) }
	assert cells != unsafe { nil }
	unsafe {
		cells[0] = AllocatorAlignedCell{ value: 17 }
		cells[1] = AllocatorAlignedCell{ value: 23 }
	}
	assert unsafe { cells[0].value + cells[1].value } == 40
	release_c_aligned_cells(cells)
}

fn test_aligned_array_cast_and_literal_share_the_v_allocation_family() {
	mut cells := unsafe { &AllocatorAlignedCells(malloc(sizeof(AllocatorAlignedCells))) }
	assert cells != unsafe { nil }
	unsafe {
		cells[0] = AllocatorAlignedCell{ value: 31 }
		cells[1] = AllocatorAlignedCell{ value: 37 }
	}
	assert unsafe { cells[0].value + cells[1].value } == 68
	release_v_aligned_cells(cells)
	release_v_aligned_cells(&AllocatorAlignedCells{})
}
