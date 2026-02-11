// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
module sfons

import fontstash
import sokol.memory

pub struct SfonsAllocator {
pub:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

pub struct SfonsDesc {
pub:
	width     int
	height    int
	allocator SfonsAllocator
}

pub fn create(width int, height int, flags int) &fontstash.Context {
	return unsafe { nil }
}

pub fn destroy(ctx &fontstash.Context) {
}

pub fn rgba(r u8, g u8, b u8, a u8) u32 {
	return (u32(a) << 24) | (u32(b) << 16) | (u32(g) << 8) | u32(r)
}

pub fn flush(ctx &fontstash.Context) {
}

fn is_power_of_two(x int) bool {
	return x in [2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768]
}
