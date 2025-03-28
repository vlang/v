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
	status string
}

// status returns a new Status struct with the given message, status, and code
pub fn status(msg string, status http.Status) Status {
	return Status{
		msg:    msg
		resp:   status.str()
		code:   status.int()
		status: 'success'
	}
}
