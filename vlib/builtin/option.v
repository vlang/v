// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// Option is the base of V's internal option return system.
struct Option {
	state u8 // 0 - ok; 2 - none; 1 - ?
	err   IError = none__
	// Data is trailing after err
	// and is not included in here but in the
	// derived Option_xxx types
}

// option is the base of V's internal option return system.
struct _option {
	state u8
	err   IError = none__
	// Data is trailing after err
	// and is not included in here but in the
	// derived _option_xxx types
}

fn _option_none(data voidptr, mut option _option, size int) {
	unsafe {
		*option = _option{
			state: 2
		}
		// use err to get the end of OptionBase and then memcpy into it
		vmemcpy(&u8(&option.err) + sizeof(IError), data, size)
	}
}

fn _option_ok(data voidptr, mut option _option, size int) {
	unsafe {
		*option = _option{}
		// use err to get the end of OptionBase and then memcpy into it
		vmemcpy(&u8(&option.err) + sizeof(IError), data, size)
	}
}

fn _option_clone(current &_option, mut option _option, size int) {
	unsafe {
		*option = _option{
			state: current.state
			err:   current.err
		}
		// use err to get the end of OptionBase and then memcpy into it
		vmemcpy(&u8(&option.err) + sizeof(IError), &u8(&current.err) + sizeof(IError),
			size)
	}
}

//

const none__ = IError(&None__{})

struct None__ {
	Error
}

fn (_ None__) str() string {
	return 'none'
}

pub fn (_ none) str() string {
	return 'none'
}
