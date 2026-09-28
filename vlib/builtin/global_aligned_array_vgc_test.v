// vtest vflags: -gc vgc
@[has_globals]
module builtin

@[aligned: 512]
struct VgcGlobalAlignedCell {
	value string
}

__global vgc_global_aligned_cells = &[1]VgcGlobalAlignedCell{init: VgcGlobalAlignedCell{
	value: 'managed field'.repeat(16)
}}

fn test_global_aligned_array_is_scanned_by_vgc() {
	$if vgc ? {
		assert usize(voidptr(vgc_global_aligned_cells)) % 512 == 0
		span := vgc_find_span(voidptr(vgc_global_aligned_cells))
		assert span != unsafe { nil }
		assert !span.noscan
		vgc_gc_start()
		assert unsafe { vgc_global_aligned_cells[0].value } == 'managed field'.repeat(16)
	}
}
