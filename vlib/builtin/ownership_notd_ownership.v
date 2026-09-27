module builtin

// OwnershipIErrorPayload matches the backend-neutral leading object pointer in IError.
struct OwnershipIErrorPayload {
	payload voidptr
}

fn drop_owned_result_error(err IError) {
	raw_err := unsafe { &OwnershipIErrorPayload(&err) }
	none_err := unsafe { &OwnershipIErrorPayload(&none__) }
	sentinel_err := unsafe { &OwnershipIErrorPayload(&error_sentinel) }
	if raw_err.payload == unsafe { nil } {
		message := err.msg()
		unsafe { message.free() }
		return
	}
	if raw_err.payload == none_err.payload || raw_err.payload == sentinel_err.payload {
		return
	}
	drop_owned_result_error_interface(err)
}

fn drop_owned_map_key[K](key K) {
	drop_owned(key)
}

// drop_owned destroys an owned value outside ownership mode.
//
// Ownership builds replace this generic fallback with the compiler intrinsic that
// destroys the value in place.
@[manualfree]
pub fn drop_owned[T](value T) {
	mut owned := value
	$if T.unaliased_typ is $interface {
		drop_owned_interface(owned)
	} $else $if T is OwnershipDrop {
		owned.drop()
	} $else $if T.unaliased_typ is string {
		unsafe { owned.free() }
	} $else $if T.unaliased_typ is $option {
		if payload := owned {
			drop_owned(payload)
		} else {
			drop_owned_result_error(owned.err)
		}
	} $else $if T.unaliased_typ is $sumtype {
		$for variant in T.variants {
			if owned is variant {
				drop_owned(owned)
			}
		}
	} $else $if T.unaliased_typ is $array_dynamic {
		mut raw_array := unsafe { &array(&owned) }
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
			drop_owned_map_key(key)
			drop_owned(item)
		}
		unsafe { owned.free() }
	} $else $if T.unaliased_typ.payload_type != 0 {
		// Container types also expose element metadata through payload_type. Check
		// them first; after those branches, a remaining payload wrapper is a result.
		if payload := owned {
			drop_owned(payload)
		} else {
			drop_owned_result_error(owned.err)
		}
	} $else $if T.unaliased_typ is $struct {
		$for field in T.fields {
			drop_owned(owned.$(field.name))
		}
	}
}
