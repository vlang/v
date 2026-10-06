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

pub fn outer_idx[T]() int {
	return rt.idx[&Cell[T]]()
}

pub fn local_names() string {
	return typeof[&Node]().name + ' ' + typeof[Box[int]]().name
}

pub fn generic_names[T]() string {
	return typeof[&Node]().name + ' ' + typeof[Box[T]]().name
}

pub fn outer_name[T]() string {
	return rt.name[&Wrap[T]]()
}

pub fn plain_name() string {
	return rt.name[&Wrap[int]]()
}
