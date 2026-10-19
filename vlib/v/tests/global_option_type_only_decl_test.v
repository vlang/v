@[has_globals]
module main

struct GlobalOptionPoint {
mut:
	x int
	y int
}

@[heap]
struct GlobalOptionHeapPoint {
mut:
	v int
}

type GlobalOptionAlias = ?i64

__global global_alias_opt GlobalOptionAlias
__global global_opt ?int
__global global_opt_str ?string
__global global_opt_struct ?GlobalOptionPoint
__global global_opt_ref ?&GlobalOptionPoint
__global global_opt_heap_ref ?&GlobalOptionHeapPoint
__global global_res !int

__global (
	global_block_opt ?u64
	global_block_res !string
)

fn set_globals() {
	global_opt = 7
	global_opt_str = 'v'
	global_opt_struct = GlobalOptionPoint{
		x: 1
		y: 2
	}
	global_opt_ref = &GlobalOptionPoint{
		x: 3
		y: 4
	}
	global_opt_heap_ref = &GlobalOptionHeapPoint{
		v: 5
	}
	global_res = 6
}

fn test_global_option_assigned_and_read() {
	set_globals()
	assert global_opt or { 0 } == 7
	assert global_opt_str or { '' } == 'v'
	assert (global_opt_struct or { GlobalOptionPoint{} }).x == 1
	assert (global_opt_ref or { &GlobalOptionPoint{} }).y == 4
	assert (global_opt_heap_ref or { &GlobalOptionHeapPoint{} }).v == 5
	assert global_res or { 0 } == 6
}

fn test_global_option_reassigned_none() {
	set_globals()
	global_opt = none
	global_opt_str = none
	global_opt_struct = none
	global_opt_ref = none
	global_opt_heap_ref = none
	global_res = none
	assert global_opt == none
	assert global_opt_str == none
	assert global_opt_struct == none
	assert global_opt_ref == none
	assert global_opt_heap_ref == none
	assert global_res == none
}

fn test_global_option_block_declarations() {
	global_block_opt = u64(12)
	global_block_res = 'ready'
	assert global_block_opt or { u64(0) } == u64(12)
	assert global_block_res or { '' } == 'ready'
	global_block_opt = none
	assert global_block_opt == none
	global_block_opt = u64(19)
	assert global_block_opt or { u64(0) } == u64(19)
}

fn test_spawned_optional_capture_keeps_its_wrapper() {
	value := ?int(17)
	callback := fn [value] () int {
		return value or { 0 }
	}
	worker := spawn callback()
	assert worker.wait() == 17
}

fn global_alias_option_value(present bool) ?i64 {
	if !present {
		return none
	}
	return i64(23)
}

fn test_global_option_alias() {
	global_alias_opt = global_alias_option_value(true)
	assert global_alias_opt or { i64(0) } == i64(23)
	global_alias_opt = global_alias_option_value(false)
	assert global_alias_opt == none
}
