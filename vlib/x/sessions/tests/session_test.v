import time
import x.sessions

const max_age = time.second
const secret = 'session_test'.bytes()

pub struct User {
	name string
	age  int
}

const default_user = User{
	name: 'john'
	age:  99
}

fn test_session_id() {
	unverified_sid, sid_with_hmac := sessions.new_session_id(secret)
	verified_sid, valid := sessions.verify_session_id(sid_with_hmac, secret)

	assert unverified_sid == verified_sid
	assert valid == true
}

fn test_forged_signature_rejected() {
	// Create a valid session ID
	sid, _ := sessions.new_session_id(secret)

	// Forge a cookie with valid session ID but invalid signature
	forged_cookie := '${sid}.INVALID_SIGNATURE'
	verified_sid, valid := sessions.verify_session_id(forged_cookie, secret)

	// Forged signature must be rejected
	assert valid == false
	assert verified_sid == sid
}

fn test_wrong_secret_rejected() {
	// Create a session with one secret
	_, signed_cookie := sessions.new_session_id(secret)

	// Try to verify with a different secret
	wrong_secret := 'wrong_secret'.bytes()
	_, valid := sessions.verify_session_id(signed_cookie, wrong_secret)

	// Must be rejected when using wrong secret
	assert valid == false
}

fn test_malformed_cookie_rejected() {
	// Cookie without signature separator
	_, valid1 := sessions.verify_session_id('just_a_session_id', secret)
	assert valid1 == false

	// Empty cookie
	_, valid2 := sessions.verify_session_id('', secret)
	assert valid2 == false

	// Cookie with empty parts
	_, valid3 := sessions.verify_session_id('.', secret)
	assert valid3 == false
}
