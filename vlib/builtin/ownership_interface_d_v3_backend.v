module builtin

// OwnershipV3InterfacePayload matches the v3 runtime interface header.
struct OwnershipV3InterfacePayload {
	payload  voidptr
	typ      int
	is_boxed bool
}

fn drop_owned_v3_interface[T](value T) {
	$if T.unaliased_typ is $interface {
		mut owned := value
		raw_interface := unsafe { &OwnershipV3InterfacePayload(&owned) }
		if raw_interface.is_boxed {
			// The v3 C backend recognizes builtin.drop_owned as an intrinsic and uses
			// the interface type id to destroy the concrete value before freeing its box.
			drop_owned(owned)
		}
	}
}

fn drop_owned_interface[T](value T) {
	drop_owned_v3_interface(value)
}

fn drop_owned_result_error_interface(err IError) {
	// Pointer-backed errors remain borrowed; only boxed concrete values are owned.
	mut owned := err
	raw_interface := unsafe { &OwnershipV3InterfacePayload(&owned) }
	if raw_interface.is_boxed {
		drop_owned(owned)
	}
}
