module builtin

interface OwnershipDrop {
mut:
	drop()
}

// OwnershipV3InterfacePayload matches the v3 runtime interface header.
struct OwnershipV3InterfacePayload {
	payload  voidptr
	typ      int
	is_boxed bool
}

fn drop_owned_v3_interface[T](value T, force_owned bool) {
	$if T.unaliased_typ is $interface {
		mut owned := value
		raw_interface := unsafe { &OwnershipV3InterfacePayload(&owned) }
		if force_owned || raw_interface.is_boxed {
			payload := raw_interface.payload
			if mut owned is OwnershipDrop {
				owned.drop()
			} else {
				unsafe { owned.free() }
			}
			if payload != unsafe { nil } {
				unsafe { free(payload) }
			}
		}
	}
}

fn drop_owned_interface[T](value T) {
	drop_owned_v3_interface(value, false)
}

fn drop_owned_result_error_interface(err IError) {
	// Failed results own pointer-backed error payloads even though ordinary interface
	// conversions treat an unboxed pointer as borrowed.
	drop_owned_v3_interface(err, true)
}
