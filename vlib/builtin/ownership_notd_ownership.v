module builtin

// drop_owned is a no-op outside ownership mode.
//
// Ownership containers can call this generic helper unconditionally; ownership
// builds replace it with the compiler intrinsic that destroys the value in place.
pub fn drop_owned[T](value T) {
	_ = value
}
