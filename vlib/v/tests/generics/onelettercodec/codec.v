module onelettercodec

// Encoder has generic methods whose specializations are named after their type argument.
pub struct Encoder {}

// encode returns the type name of `v`.
pub fn (e Encoder) encode[T](v T) string {
	return typeof(v).name
}

// wrap returns a one-element array containing `v`.
pub fn (e Encoder) wrap[T](v T) []T {
	mut out := []T{}
	out << v
	return out
}

// encode forwards `v` to the generic `Encoder.encode` method.
pub fn encode[T](v T) string {
	e := Encoder{}
	return e.encode(v)
}

// name returns the type name of `v`.
pub fn name[T](v T) string {
	return typeof(v).name
}

// Holder is a generic struct with a method specialized per type argument.
pub struct Holder[T] {
pub:
	value T
}

// name returns the type name of the held value.
pub fn (h Holder[T]) name() string {
	return typeof(h.value).name
}
