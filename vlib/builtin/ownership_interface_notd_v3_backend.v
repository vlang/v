module builtin

interface OwnershipDrop {
mut:
	drop()
}

// OwnershipRegularInterfacePayload matches the leading object pointer shared by
// regular-backend interface values.
struct OwnershipRegularInterfacePayload {
	payload voidptr
}

fn drop_owned_interface[T](value T) {
	$if T.unaliased_typ is $interface {
		_ = value
		$compile_error('drop_owned for interface payloads requires the v3 backend because the regular interface ABI does not record whether its object is owned')
	}
}

fn drop_owned_result_error_interface(err IError) {
	mut owned := err
	payload := unsafe { (&OwnershipRegularInterfacePayload(&owned)).payload }
	if mut owned is OwnershipDrop {
		owned.drop()
		if payload != unsafe { nil } {
			unsafe { free(payload) }
		}
	} else {
		// The regular backend's IError.free() owns its pointer-backed error object.
		unsafe { owned.free() }
	}
}
