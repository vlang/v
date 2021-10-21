// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

pub struct TimeParseError {
	msg  string
	code int
}

fn error_invalid_time(code int) IError {
	return TimeParseError{
		msg: 'Invalid time format code: $code'
		code: code
	}
}
