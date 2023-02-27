module builtin

fn __memory_grow(size isize) isize
fn __memory_fill(dest &u8, value isize, size isize)
fn __memory_copy(dest &u8, src &u8, size isize)
