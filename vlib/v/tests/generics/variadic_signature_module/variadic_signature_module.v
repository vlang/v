module variadic_signature_module

pub struct Param {
pub:
	n int
}

pub struct Gate[T] {
pub:
	value T
}

// choose returns the first argument from a generic receiver's variadic method.
pub fn (g Gate[T]) choose(args ...Param) int {
	return args[0].n
}
