fn get_chunkmap_at_coords(mapp []Chunk) [chunk_size][chunk_size]u64 {
	return mapp[0].id_map
}

const chunk_size = 100

struct Chunk {
	id_map [chunk_size][chunk_size]u64
}

fn test_main() {
	t := Chunk{}
	assert t.id_map[0].len == 100
	assert t.id_map.len == 100
}
