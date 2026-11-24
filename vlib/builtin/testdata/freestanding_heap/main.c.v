module main

// Exercise the canonical bare runtime hook through the same C ABI used by
// generated native allocation helpers, without making it a public V API.
fn C.__malloc_align(size uint, alignment uint) voidptr

__global heap_workers_ready u32
__global heap_workers_go u32
__global heap_worker_results [2]voidptr

// Keep verification active in optimized standalone executables.
fn require_heap(condition bool) {
	if !condition { exit(92) }
}

fn heap_worker(worker u64) {
	for {
		old := vlibc_load_u32(&heap_workers_ready)
		if vlibc_cas_u32(&heap_workers_ready, old, old + 1) { break }
	}
	for vlibc_load_u32(&heap_workers_go) == 0 {
	}
	// These raw slots are filled before each read; nil owns no allocation.
	mut blocks := unsafe { [32]voidptr{} }
	mut sizes := [32]int{}
	for round in 0 .. 64 {
		for i in 0 .. blocks.len {
			size := 1 + (i * 137 + round * 17) % 8192
			address := unsafe { malloc(size) }
			require_heap(address != unsafe { nil })
			for j in 0 .. size {
				unsafe { address[j] = u8(worker + u64(j)) }
			}
			blocks[i] = address
			sizes[i] = size
		}
		for i in 0 .. blocks.len {
			address := unsafe { realloc_data(blocks[i], sizes[i], sizes[i] + 257) }
			require_heap(address != unsafe { nil })
			for j in 0 .. sizes[i] {
				require_heap(unsafe { address[j] } == u8(worker + u64(j)))
			}
			unsafe { free(address) }
		}
	}
	address := unsafe { malloc(32) }
	for i in 0 .. 32 {
		unsafe { address[i] = u8(worker + u64(i)) }
	}
	heap_worker_results[int(worker)] = address
}

fn main() {
	mut first, first_error := vlibc_thread_spawn(heap_worker, 0)
	mut second, second_error := vlibc_thread_spawn(heap_worker, 1)
	require_heap(first_error == .enoerror && second_error == .enoerror)
	for vlibc_load_u32(&heap_workers_ready) != 2 {
	}
	vlibc_store_u32(&heap_workers_go, 1)
	first_join := vlibc_thread_join(mut first)
	second_join := vlibc_thread_join(mut second)
	require_heap(first_join == .enoerror && second_join == .enoerror)
	for worker in 0 .. 2 {
		address := unsafe { &u8(heap_worker_results[worker]) }
		for i in 0 .. 32 {
			require_heap(unsafe { address[i] } == u8(worker + i))
		}
		unsafe { free(address) }
	}
	for alignment in [uint(1), 2, 4, 8, 16, 32, 64, 4096] {
		for size in [1, 17, 128, 4097] {
			address := unsafe { &u8(C.__malloc_align(uint(size), alignment)) }
			require_heap(address != unsafe { nil })
			require_heap(uint(address) % alignment == 0)
			for i in 0 .. size {
				unsafe { address[i] = u8(i) }
			}
			grown := unsafe { realloc_data(address, size, size + 1024) }
			for i in 0 .. size {
				require_heap(unsafe { grown[i] } == u8(i))
			}
			unsafe { free(grown) }
		}
	}
	require_heap(C.__malloc_align(16, 0) == unsafe { nil })
	require_heap(C.__malloc_align(16, 3) == unsafe { nil })
	require_heap(C.__malloc_align(~uint(0), 64) == unsafe { nil })
	require_heap(C.calloc((~uint(0) / 2) + 1, 2) == unsafe { nil })
	println('freestanding heap passed')
}
