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
