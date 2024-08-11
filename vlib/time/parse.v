// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// TimeParseError represents a time parsing error.
pub struct TimeParseError {
	Error
	code    int
	message string
}

// msg implements the `IError.msg()` method for `TimeParseError`.
pub fn (err TimeParseError) msg() string {
	return 'Invalid time format code: ${err.code}, error: ${err.message}'
}

fn error_invalid_time(code int, message string) IError {
	return TimeParseError{
		code:    code
		message: message
	}
}
