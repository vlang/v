@[has_globals]
module globals28888

pub struct Point {
pub:
	x u64
	y u32
}

__global bar [4]u64
__global point Point
__global count u16

@[export: 'globals28888_exported_blob']
__global blob [5]u16

const bar_bytes = sizeof(bar)

// bar_size returns the size of the unqualified module global `bar`.
pub fn bar_size() usize {
	return sizeof(bar)
}

// point_size returns the size of the struct-typed module global `point`.
pub fn point_size() usize {
	return sizeof(point)
}

// sum_size combines `sizeof` of two module globals in one expression.
pub fn sum_size() usize {
	return sizeof(bar) + sizeof(count)
}

// const_size returns a module const initialized from `sizeof` of a global.
pub fn const_size() usize {
	return bar_bytes
}

// generic_size mixes `sizeof` of a module global with a generic parameter.
pub fn generic_size[T](value T) usize {
	return sizeof(bar) + sizeof(value)
}

// exported_size returns the size of a global declared under an export name.
pub fn exported_size() usize {
	return sizeof(blob)
}
