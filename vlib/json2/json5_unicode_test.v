module json2

// The oracle counts and FNV-1a checksums come directly from Unicode 15.0.0
// UnicodeData.txt, independently of the generated range lookup. The generator in
// Unicode table generated; regeneration tooling removed (was Python).
fn test_json5_identifier_categories_match_unicode_data() {
	mut start_count := 0
	mut part_count := 0
	mut start_hash := u64(0xcbf29ce484222325)
	mut part_hash := u64(0xcbf29ce484222325)
	for code in 0 .. 0x110000 {
		start := json5_is_ident_start(code)
		part := json5_is_ident_part(code)
		start_count += int(start)
		part_count += int(part)
		start_hash = (start_hash ^ u64(start)) * u64(0x100000001b3)
		part_hash = (part_hash ^ u64(part)) * u64(0x100000001b3)
	}
	assert start_count == 136342
	assert part_count == 139470
	assert start_hash == u64(8722130277568546833)
	assert part_hash == u64(2985726584840843653)
	for code in [-1, 0x110000] {
		assert !json5_is_ident_start(code)
		assert !json5_is_ident_part(code)
	}
}
