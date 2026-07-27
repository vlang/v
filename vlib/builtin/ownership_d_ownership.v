module builtin

// drop_owned destroys an owned value in place.
//
// This is an ownership-mode compiler intrinsic used by ownership containers
// whose payload type is only known after generic specialization.
pub fn drop_owned[T](value T) {
	_ = value
}
