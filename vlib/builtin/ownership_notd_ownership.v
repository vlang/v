module builtin

interface OwnershipDrop {
mut:
	drop()
}

// OwnershipSumPayload matches the leading active-variant pointer in the runtime sum layout.
struct OwnershipSumPayload {
	payload voidptr
}

// OwnershipInterfacePayload matches the leading object pointer in the runtime
// interface layout.
struct OwnershipInterfacePayload {
	payload voidptr
}

fn drop_owned_result_error(err IError) {
	raw_err := unsafe { &C.IError(&err) }
	none_err := unsafe { &C.IError(&none__) }
	sentinel_err := unsafe { &C.IError(&error_sentinel) }
	if raw_err._object == unsafe { nil } {
		message := err.msg()
		unsafe { message.free() }
		return
	}
	if raw_err._object == none_err._object || raw_err._object == sentinel_err._object {
		return
	}
	drop_owned(err)
}

// drop_owned destroys an owned value outside ownership mode.
//
// Ownership builds replace this generic fallback with the compiler intrinsic that
// destroys the value in place.
@[manualfree]
pub fn drop_owned[T](value T) {
	mut owned := value
	$if T.unaliased_typ is $interface {
		payload := unsafe { (&OwnershipInterfacePayload(&owned)).payload }
		if mut owned is OwnershipDrop {
			owned.drop()
		} else {
			unsafe { owned.free() }
		}
		if payload != unsafe { nil } {
			unsafe { free(payload) }
		}
	} $else $if T is OwnershipDrop {
		owned.drop()
	} $else $if T.unaliased_typ is string {
		unsafe { owned.free() }
	} $else $if T.unaliased_typ is $option {
		if payload := owned {
			drop_owned(payload)
		}
	} $else $if T.unaliased_typ.payload_type != 0 {
		// After the option branch, the remaining wrapper with a payload type is a result.
		if payload := owned {
			drop_owned(payload)
		} else {
			drop_owned_result_error(owned.err)
		}
	} $else $if T.unaliased_typ is $sumtype {
		payload := unsafe { (&OwnershipSumPayload(&owned)).payload }
		$for variant in T.variants {
			if owned is variant {
				drop_owned(owned)
			}
		}
		if payload != unsafe { nil } {
			unsafe { free(payload) }
		}
	} $else $if T.unaliased_typ is $array_dynamic {
		mut raw_array := unsafe { &array(owned) }
		if !raw_array.flags.has(.is_slice) {
			for i in 0 .. owned.len {
				drop_owned(owned[i])
			}
			unsafe { raw_array.free() }
		}
	} $else $if T.unaliased_typ is $array_fixed {
		for i in 0 .. owned.len {
			drop_owned(owned[i])
		}
	} $else $if T.unaliased_typ is $map {
		for key, item in owned {
			drop_owned(key)
			drop_owned(item)
		}
		unsafe { owned.free() }
	} $else $if T.unaliased_typ is $struct {
		$for field in T.fields {
			drop_owned(owned.$(field.name))
		}
	}
}
