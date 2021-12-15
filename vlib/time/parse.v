// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

pub struct TimeParseError {
	BaseError
	code int
}

fn (err TimeParseError) msg() string {
	return 'Invalid time format code: $err.code'
}

fn (err TimeParseError) code() int {
	return err.code
}

fn error_invalid_time(code int) IError {
	return TimeParseError{
		code: code
	}
}
