module builtin

// freestanding_bsd_exports_d_freestanding.v: posix_memalign, which the generated
// C's v3_aligned_memdup reaches for, for the non-Linux `*_bare` runtimes; their
// pow/fabs/floor come from freestanding_math_d_freestanding.c.v.
// linux_bare/exports_d_freestanding.v carries the Linux set. macOS is left out:
// a freestanding macOS program still links libSystem, which provides it.
$if freestanding && !i386 && (freebsd || netbsd || dragonfly || solaris) {
	// bf_posix_memalign returns `size` bytes at an address that is a multiple of
	// `alignment`, which must be a power of two and a multiple of the pointer size.
	// The block comes from the runtime's dlmalloc, so the exported free releases it.
	@[export: 'posix_memalign']
	@[markused]
	fn bf_posix_memalign(memptr &voidptr, alignment uint, size uint) int {
		if alignment == 0 || alignment & (alignment - 1) != 0
			|| alignment % uint(sizeof(voidptr)) != 0 {
			return 22 // EINVAL
		}
		if !global_allocator_ready {
			init_global_allocator()
		}
		n := if size == 0 { uint(1) } else { size }
		p := unsafe { global_allocator.memalign(alignment, n) }
		if p == unsafe { nil } {
			return 12 // ENOMEM
		}
		unsafe {
			*memptr = p
		}
		return 0
	}
}
