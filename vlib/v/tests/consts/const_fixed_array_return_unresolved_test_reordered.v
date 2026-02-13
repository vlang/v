fn get_chunkmap_at_coords(mapp []Chunk) [chunk_size][chunk_size]u64 {
	return mapp[0].id_map
}

const chunk_size = 100

struct Chunk {
	id_map [chunk_size][chunk_size]u64
}

fn main() {
	x := get_chunkmap_at_coords([]Chunk{len: 1})
	assert x.len == 100
	assert x[0].len == 100
}
