module typeof_parameterized_model

import typeof_parameterized_runtime as rt

pub struct Cell[T] {
	value T
}

pub struct Node {}

pub struct Box[T] {}

pub struct Wrap[T] {
	value T
}

// outer_idx queries an imported generic for a local cell pointer type.
pub fn outer_idx[T]() int {
	return rt.idx[&Cell[T]]()
}

// local_names reports explicit local composite type names.
pub fn local_names() string {
	return typeof[&Node]().name + ' ' + typeof[Box[int]]().name
}

// generic_names reports local composite type names after substitution.
pub fn generic_names[T]() string {
	return typeof[&Node]().name + ' ' + typeof[Box[T]]().name
}

// outer_name transports a local wrap pointer type to another module.
pub fn outer_name[T]() string {
	return rt.name[&Wrap[T]]()
}

// plain_name reports an explicitly instantiated transported type.
pub fn plain_name() string {
	return rt.name[&Wrap[int]]()
}
