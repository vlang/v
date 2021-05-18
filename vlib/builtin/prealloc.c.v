module builtin

__global g_m2_buf &byte
__global g_m2_ptr &byte
__global nr_mallocs = int(0)

[unsafe]
fn prealloc_vinit() {
	unsafe {
		g_m2_buf = C.malloc(250 * 1000 * 1000)
		g_m2_ptr = g_m2_buf
		C.atexit(prealloc_vcleanup)
	}
}

[unsafe]
fn prealloc_vcleanup() {
	unsafe {
		$if prealloc_stats ? {
			eprintln('> g_m2_buf: ${voidptr(g_m2_buf)} | g_m2_ptr: ${voidptr(g_m2_ptr)} | nr_mallocs: $nr_mallocs | diff: ${u64(g_m2_ptr) - u64(g_m2_buf)} bytes')
		}
		C.free(g_m2_buf)
	}
}

[unsafe]
fn prealloc_malloc(n int) &byte {
	unsafe {
		mut res := &byte(0)
		res = g_m2_ptr
		g_m2_ptr += n
		nr_mallocs++
		return res
	}
}

[unsafe]
fn prealloc_realloc(old_data &byte, old_size int, new_size int) &byte {
	unsafe {
		new_ptr := prealloc_malloc(new_size)
		min_size := if old_size < new_size { old_size } else { new_size }
		C.memcpy(new_ptr, old_data, min_size)
		return new_ptr
	}
}

[unsafe]
pub fn prealloc_calloc(n int) &byte {
	unsafe {
		new_ptr := prealloc_malloc(n)
		C.memset(new_ptr, 0, n)
		return new_ptr
	}
}
