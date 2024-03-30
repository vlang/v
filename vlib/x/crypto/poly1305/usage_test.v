import encoding.hex
import x.crypto.poly1305

// perform test for oneshot function for poly1305 mac creation and verifying it.
fn test_fn_create_and_verify_tag() ! {
	// Sample messages from RFC A.3 Test vector 2
	// set secure key
	key := hex.decode('0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e')!
	// messages to be authenticated
	msg := 'Any submission to the IETF intended by the Contributor for publication as all or part of an IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is considered an "IETF Contribution". Such statements include oral statements in IETF sessions, as well as written and electronic communications made at any time or place, which are addressed to'
		.bytes()

	// lets creates buffers output and build the tag
	mut out := []u8{len: poly1305.tag_size}
	poly1305.create_tag(mut out, msg, key)!
	// we have expected tag
	expected_tag := hex.decode('36e5f6b5c5e06070f0efca96227a863e')!
	assert out == expected_tag

	// lets assume we have a tag, and want to verify this
	status := poly1305.verify_tag(expected_tag, msg, key)
	assert status == true
}

// perform test for instance based method.
fn test_create_and_verify_tag_with_poly1305_instance() ! {
	// Sample messages from RFC A.3 Test vector 2
	// set secure key
	key := hex.decode('0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e')!
	// messages to be authenticated
	msg := 'Any submission to the IETF intended by the Contributor for publication as all or part of an IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is considered an "IETF Contribution". Such statements include oral statements in IETF sessions, as well as written and electronic communications made at any time or place, which are addressed to'
		.bytes()

	// lets creates buffers output and build the tag
	mut out := []u8{len: poly1305.tag_size}
	// we have expected tag
	expected_tag := hex.decode('36e5f6b5c5e06070f0efca96227a863e')!

	// create a new Poly1305 instance
	mut po := poly1305.new(key)!
	po.update(msg)
	// you can check for the current state, we have expected tag
	// dont call .verify after calling .finish, when its happens,
	// you should reinit it with new key to make it usable.
	valid_current_tag := po.verify(expected_tag)
	assert valid_current_tag == true

	// after call to .finish, you should not use the instance
	po.finish(mut out)
	assert out == expected_tag
}

// In this test, we perform test for methods based Poly1305 instance in incremental manner,
// updates state by multiples block of messages by calling .update method of the Poly1305 instance
fn test_create_and_verify_tag_with_poly1305_instance_in_incremental_updates() ! {
	// Sample messages from RFC A.3 Test vector 2
	// set secure key
	key := hex.decode('0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e')!
	// messages to be authenticated
	// msg := 'Any submission to the IETF intended by the Contributor for publication as all or part of an IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is considered an "IETF Contribution". Such statements include oral statements in IETF sessions, as well as written and electronic communications made at any time or place, which are addressed to'
	//	.bytes()

	// we split above message to five msg block
	msg0 := 'Any submission to the IETF intended by the Contributor for publication as all '.bytes()
	msg1 := 'or part of an IETF Internet-Draft or RFC and any statement made within '.bytes()
	msg2 := 'the context of an IETF activity is considered an "IETF Contribution". Such statements '.bytes()
	msg3 := 'include oral statements in IETF sessions, as well as written and electronic '.bytes()
	msg4 := 'communications made at any time or place, which are addressed to'.bytes()
	// lets creates buffers output and build the tag
	mut out := []u8{len: poly1305.tag_size}
	// we have expected tag
	expected_tag := hex.decode('36e5f6b5c5e06070f0efca96227a863e')!

	// create a new Poly1305 instance
	mut po := poly1305.new(key)!
	po.update(msg0)
	po.update(msg1)
	po.update(msg2)
	po.update(msg3)
	po.update(msg4)
	// after call to .finish, you should not use the instance
	po.finish(mut out)
	assert out == expected_tag
}
