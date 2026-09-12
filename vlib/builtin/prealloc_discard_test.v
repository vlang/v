module builtin

fn test_prealloc_discard_preserves_neighbors_and_writable_reservation() {
	mut bytes := []u8{len: 300_013, init: 0xa5}
	start := 10_003
	end := 210_017
	unsafe { prealloc_discard_pages(&bytes[start], usize(end - start)) }
	for i in 0 .. start {
		assert bytes[i] == 0xa5
	}
	for i in end .. bytes.len {
		assert bytes[i] == 0xa5
	}
	for i in start .. end {
		bytes[i] = u8(i)
	}
	for i in start .. end {
		assert bytes[i] == u8(i)
	}
}

fn test_prealloc_realloc_preserves_contents_and_neighbor_allocation() {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		data := unsafe { malloc(131_101) }
		unsafe { vmemset(data, 0xa5, 131_101) }
		neighbor := []u8{len: 65_539, init: 0x3c}
		grown := unsafe { realloc_data(data, 131_101, 262_207) }
		for i in 0 .. 131_101 {
			assert unsafe { grown[i] } == 0xa5
		}
		for byte in neighbor {
			assert byte == 0x3c
		}
		unsafe { prealloc_scope_end(scope) }
	}
}

fn test_noslices_growth_preserves_tracked_slice() {
	mut bytes := []u8{len: 131_101, init: 0xa5}
	unsafe { bytes.flags.set(.noslices) }
	borrowed := bytes[0..bytes.len]
	bytes << []u8{len: 131_101, init: 0x3c}
	assert borrowed.len == 131_101
	for byte in borrowed {
		assert byte == 0xa5
	}
	assert bytes[131_101] == 0x3c
}
