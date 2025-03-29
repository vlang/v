module status

import net.http

// Status is a struct that contains the message, status, code, and status of the request
pub struct Status {
	// msg is the message that will be returned to the user
	msg string
	// resp is the status that will be returned to the user
	resp string
	// code is the status code that will be returned to the user
	code int
	// status is the status of the request
	status http.Status
}

// status returns a new Status struct with the given message, status, and code
pub fn new_status(msg string, status http.Status) Status {
	return Status{
		msg:    msg
		resp:   status.str()
		code:   status.int()
		status: status
	}
}

// resp_code returns the status code and message as a string
// for example: "200 OK"
// this is used to return the status code and message to the user
pub fn (s Status) resp_code() string {
	return '${s.code} ${s.resp}'
}

pub fn (s Status) is_success() bool {
	if s.status.is_success() {
		return true
	} else {
		return false
	}
}

pub fn (s Status) is_ok() bool {
	if s.status.is_success() {
		return true
	} else {
		return false
	}
}

pub fn (s Status) is_error() bool {
	if s.status.is_error() {
		return true
	} else {
		return false
	}
}

pub fn (s Status) is_valid() bool {
	if s.status.is_valid() {
		return true
	} else {
		return false
	}
}
