// Simple arena allocator
struct ArenaChunk {
mut:
	next ?&ArenaChunk
	used int
	cap  int
	data byteptr
}

struct Arena {
mut:
	head ?&ArenaChunk
}

fn arena_init(mut arena Arena, first_capacity int) {
	chunk := &ArenaChunk{
		next: 0
		used: 0
		cap:  first_capacity
		data: unsafe { malloc(first_capacity) }
	}
	arena.head = chunk
}

fn test_main() {
	mut green_arena := Arena{}
	arena_init(mut green_arena, 64 * 1024)
}
