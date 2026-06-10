module api

pub struct MyStruct[T] {
pub:
	value T
}

pub fn make_struct[T](val T) MyStruct[T] {
	return MyStruct[T]{
		value: val
	}
}
