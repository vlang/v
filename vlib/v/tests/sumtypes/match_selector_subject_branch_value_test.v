struct SelectorPublicHandle {
	x int
}

struct SelectorPrivateHandle {
	d int
}

type SelectorHandle = SelectorPrivateHandle | SelectorPublicHandle

struct SelectorKey {
	handle SelectorHandle
}

fn selector_public_of(d int) !SelectorPublicHandle {
	if d < 0 {
		return error('bad private handle')
	}
	return SelectorPublicHandle{d * 2}
}

fn selector_export(key &SelectorKey) !int {
	point := match key.handle {
		SelectorPublicHandle { key.handle }
		SelectorPrivateHandle {
			selector_public_of(key.handle.d) or { return error('DataError: ${err.msg()}') }
		}
	}
	return point.x
}

fn test_match_branch_value_naming_a_selector_subject_keeps_its_variant_type() {
	assert selector_export(&SelectorKey{SelectorPublicHandle{3}})! == 3
	assert selector_export(&SelectorKey{SelectorPrivateHandle{5}})! == 10
	if _ := selector_export(&SelectorKey{SelectorPrivateHandle{-1}}) {
		assert false
	} else {
		assert err.msg() == 'DataError: bad private handle'
	}
}
