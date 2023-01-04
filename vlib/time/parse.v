// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// TimeParseError represents a time parsing error.
pub struct TimeParseError {
	Error
	code int
}

// msg implements the `IError.msg()` method for `TimeParseError`.
pub fn (err TimeParseError) msg() string {
	return 'Invalid time format code: ${err.code}'
}

fn error_invalid_time(code int) IError {
	return TimeParseError{
		code: code
	}
}
