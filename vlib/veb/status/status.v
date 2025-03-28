module status

pub struct Status {
	msg string
	resp string
	code int
	status string
}

pub fn ok(msg string) Status {
	return Status{
		msg: msg,
		resp: 'OK',
		code: 200,
		status: 'success',
	}
}

pub fn created(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Created',
		code: 201,
		status: 'success',
	}
}

pub fn no_content(msg string) Status {
	return Status{
		msg: msg,
		resp: 'No Content',
		code: 204,
		status: 'success',
	}
}

pub fn accepted(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Accepted',
		code: 202,
		status: 'success',
	}
}

pub fn not_modified(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Not Modified',
		code: 304,
		status: 'success',
	}
}

pub fn bad_request(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Bad Request',
		code: 400,
		status: 'error',
	}
}

pub fn not_found(msg string) Status {
	return Status{
		msg: msg
		resp: 'Not Found',
		code: 404,
		status: 'error',
	}
}

pub fn internal_server_error(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Internal Server Error',
		code: 500,
		status: 'error',
	}
}

pub fn forbidden(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Forbidden',
		code: 403,
		status: 'error',
	}
}

pub fn unauthorized(msg string) Status {
	return Status{
		msg: msg
		resp: 'Unauthorized',
		code: 401,
		status: 'error',
	}
}

pub fn conflict(msg string) Status {
	return Status{
		msg: msg,
		resp: 'Conflict',
		code: 409,
		status: 'error',
	}
}