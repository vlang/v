struct Foo {}

@[unsafe]
fn test_assign_ref_from_match() {
	var := match 0 {
		0 {
			&Foo{}
		}
		else {
			unsafe { nil }
		}
	}

	assert var != unsafe { nil }
}

@[unsafe]
fn return_ref_from_match() &Foo {
	return match 0 {
		0 {
			&Foo{}
		}
		else {
			unsafe { nil }
		}
	}
}

@[unsafe]
fn test_return_ref_from_match() {
	var := return_ref_from_match()
	assert var != unsafe { nil }
}
