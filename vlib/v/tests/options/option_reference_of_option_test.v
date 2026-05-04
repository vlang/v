fn test_reference_of_option_preserves_payload() {
	opt := ?int(-1)
	ref_opt := &opt

	assert *ref_opt? == opt?
}

fn test_reference_of_option_preserves_none() {
	opt := ?int(none)
	ref_opt := &opt

	assert ref_opt == none
}

fn test_reference_of_option_updates_original_payload() {
	mut opt := ?int(1)
	ref_opt := &opt

	unsafe {
		*ref_opt? = 2
	}
	assert opt? == 2
}
