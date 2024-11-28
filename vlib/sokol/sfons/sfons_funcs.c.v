module sfons

import fontstash
import sokol.memory

@[typedef]
pub struct C.sfons_allocator_t {
pub:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

@[typedef]
pub struct C.sfons_desc_t {
pub:
	width     int                 // initial width of font atlas texture (default: 512, must be power of 2)
	height    int                 // initial height of font atlas texture (default: 512, must be power of 2)
	allocator C.sfons_allocator_t // optional memory allocation overrides
}

fn C.sfons_create(desc &C.sfons_desc_t) &fontstash.Context
fn C.sfons_destroy(ctx &fontstash.Context)
fn C.sfons_flush(ctx &fontstash.Context)

fn C.sfons_rgba(r u8, g u8, b u8, a u8) u32
