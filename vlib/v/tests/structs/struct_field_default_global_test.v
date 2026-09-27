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

// These held the object in a local named `g`, so the field default above ran
// while a local shadowed the global it reads. V rejects such a local now, so
// the object gets its own name and what stays covered is the part that is
// still reachable: a field default evaluating a global on construction.
fn test_stack_struct_field_default_can_use_global() {
	reset_game_state()
	mut obj := StackGameObject{}
	assert obj.id == 0
}

fn test_heap_struct_field_default_can_use_global() {
	reset_game_state()
	mut obj := HeapGameObject{}
	assert obj.id == 0
}
