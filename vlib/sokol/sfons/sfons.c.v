module sfons

import fontstash
import sokol.f as _
import sokol.memory

// create a new Context/font atlas, for rendering glyphs, given its dimensions `width` and `height`
@[inline]
pub fn create(width int, height int, flags int) &fontstash.Context {
	assert is_power_of_two(width)
	assert is_power_of_two(height)
	allocator := C.sfons_allocator_t{
		alloc_fn:  memory.salloc
		free_fn:   memory.sfree
		user_data: voidptr(0x100005f0)
	}
	desc := C.sfons_desc_t{
		width:     width
		height:    height
		allocator: allocator
	}
	return C.sfons_create(&desc)
}

@[inline]
pub fn destroy(ctx &fontstash.Context) {
	C.sfons_destroy(ctx)
}

@[inline]
pub fn rgba(r u8, g u8, b u8, a u8) u32 {
	return C.sfons_rgba(r, g, b, a)
}

@[inline]
pub fn flush(ctx &fontstash.Context) {
	C.sfons_flush(ctx)
}

fn is_power_of_two(x int) bool {
	return x in [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768]
}
