@[has_globals]
module main

struct Game {
mut:
	object_id u32
}

__global (
	g Game
)

struct StackGameObject {
mut:
	id u32 = g.object_id++
}

@[heap]
struct HeapGameObject {
mut:
	id u32 = g.object_id++
}

fn reset_game_state() {
	g = Game{}
}

fn test_stack_struct_field_default_can_use_shadowed_global() {
	reset_game_state()
	mut g := StackGameObject{}
	assert g.id == 0
}

fn test_heap_struct_field_default_can_use_shadowed_global() {
	reset_game_state()
	mut g := HeapGameObject{}
	assert g.id == 0
}
