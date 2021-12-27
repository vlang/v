// This is a version of dlmalloc.c ported to V. You can find the original
// source at ftp://g.oswego.edu/pub/misc/malloc.c
//
// The original source was written by Doug Lea and released to the public domain
module dlmalloc

const (
	n_small_bins           = 32
	n_tree_bins            = 32
	small_bin_shift        = 3
	tree_bin_shift         = 8
	default_granularity    = 64 * 1024
	default_trim_threshold = 2 * 1024 * 1024
	max_release_check_rate = 4095
	malloc_alignment       = sizeof(usize) * 2
	chunk_overhead         = sizeof(usize)
	mmap_chnk_overhead     = 2 * sizeof(usize)
	min_large_size         = 1 << tree_bin_shift
	max_small_size         = min_large_size - 1
	max_small_request      = max_small_size - (malloc_alignment - 1) - chunk_overhead
	min_chunk_size         = align_up(sizeof(Chunk), malloc_alignment)
	chunk_mem_offset       = 2 * sizeof(usize)
	min_request            = min_chunk_size - chunk_overhead - 1
	top_foot_size          = align_offset_usize(chunk_mem_offset) + pad_request(sizeof(Segment)) +
		min_chunk_size
	max_request            = calc_max_request()
	mmap_foot_pad          = 4 * sizeof(usize)
)

fn min_sys_alloc_space() usize {
	return ((~0 - (dlmalloc.default_granularity + dlmalloc.top_foot_size +
		dlmalloc.malloc_alignment) + 1) & ~dlmalloc.malloc_alignment) - dlmalloc.chunk_overhead + 1
}

fn calc_max_request() usize {
	x := min_sys_alloc_space()
	y := (~dlmalloc.min_chunk_size + 1) << 2
	if x < y {
		return x
	} else {
		return y
	}
}

fn pad_request(amt usize) usize {
	return align_up(amt + dlmalloc.chunk_overhead, dlmalloc.malloc_alignment)
}

fn align_offset_usize(addr usize) usize {
	return align_up(addr, dlmalloc.malloc_alignment) - addr
}

fn is_aligned(a usize) bool {
	return a & (dlmalloc.malloc_alignment - 1) == 0
}

fn is_small(s usize) bool {
	return s >> dlmalloc.small_bin_shift < dlmalloc.n_small_bins
}

fn small_index2size(idx u32) usize {
	return usize(idx) << dlmalloc.small_bin_shift
}

fn small_index(size usize) u32 {
	return u32(size >> dlmalloc.small_bin_shift)
}

fn align_up(a usize, alignment usize) usize {
	return (a + (alignment - 1)) & ~(alignment - 1)
}

fn left_bits(x u32) u32 {
	return (x << 1) | (~(x << 1)) + 1
}

fn least_bit(x u32) u32 {
	return x & (~x + 1)
}

fn leftshift_for_tree_index(x u32) u32 {
	y := usize(x)
	if y == dlmalloc.n_tree_bins - 1 {
		return 0
	} else {
		return u32(sizeof(usize) * 8 - 1 - ((y >> 1) + dlmalloc.tree_bin_shift - 2))
	}
}

[unsafe]
fn align_as_chunk(ptr_ voidptr) &Chunk {
	ptr := usize(ptr_)
	chunk := ptr + dlmalloc.chunk_mem_offset
	return &Chunk(ptr + align_offset_usize(chunk))
}

fn request_2_size(req usize) usize {
	if req < dlmalloc.min_request {
		return dlmalloc.min_chunk_size
	} else {
		return pad_request(req)
	}
}

// In order for dlmalloc to efficently manage memory, it needs a way to communicate with the underlying paltform.
// This `Allocator` type provides an interface for this communication.
//
//
// Why not `interface?` Interfaces require memory allocation so it is simpler to pass a struct.
pub struct Allocator {
	alloc            fn (voidptr, usize) (voidptr, usize, u32)
	remap            fn (voidptr, voidptr, usize, usize, bool) voidptr
	free_part        fn (voidptr, voidptr, usize, usize) bool
	free             fn (voidptr, voidptr, usize) bool
	can_release_part fn (voidptr, u32) bool
	allocates_zeros  fn (voidptr) bool
	page_size        fn (voidptr) usize // not a constant field because some platforms might have different page sizes depending on configs
	data             voidptr
}

pub struct Dlmalloc {
	system_allocator Allocator
mut:
	// bin maps
	smallmap u32 // bin map for small bins
	treemap  u32 // bin map  for tree bins

	smallbins      [66]&Chunk // small bins, it is actually (n_small_bins + 1) * 2
	treebins       [n_tree_bins]&Chunk
	dvsize         usize
	topsize        usize
	dv             &Chunk
	top            &Chunk
	footprint      usize
	max_footprint  usize
	seg            Segment
	trim_check     u32
	least_addr     voidptr
	release_checks usize
}

struct Chunk {
	prev_foot usize
	head      usize
	prev      &Chunk
	next      &Chunk
}

struct Segment {
	base  voidptr
	size  usize
	next  &Segment
	flags u32
}

struct TreeChunk {
	Chunk
	child  [2]voidptr
	parent voidptr
	index  u32
}
