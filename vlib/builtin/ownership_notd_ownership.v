module builtin

interface OwnershipDrop {
mut:
	drop()
}

// drop_owned destroys an owned value outside ownership mode.
//
// Ownership builds replace this generic fallback with the compiler intrinsic that
// destroys the value in place.
@[manualfree]
pub fn drop_owned[T](value T) {
	mut owned := value
	$if T is OwnershipDrop {
		owned.drop()
	} $else $if T.unaliased_typ is string {
		unsafe { owned.free() }
	} $else $if T.unaliased_typ is $option {
		if payload := owned {
			drop_owned(payload)
		}
	} $else $if T.unaliased_typ is $sumtype {
		$for variant in T.variants {
			if owned is variant {
				drop_owned(owned)
			}
		}
	} $else $if T.unaliased_typ is $array_dynamic {
		mut raw_array := unsafe { &array(owned) }
		if !raw_array.flags.has(.is_slice) {
			for i in 0 .. owned.len {
				drop_owned(owned[i])
			}
		}
		unsafe { raw_array.free() }
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
