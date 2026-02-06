module builtin

// ChanState describes the result of an attempted channel transaction.
pub enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

/*
The following methods are only stubs.
The real implementation is in `vlib/sync/channels.v`
*/

// close closes the channel for further push transactions.
// closed channels cannot be pushed to, however they can be popped
// from as long as there is still objects available in the channel buffer.
pub fn (ch chan) close() {}

// try_pop returns `ChanState.success` if an object is popped from the channel.
// try_pop effectively pops from the channel without waiting for objects to become available.
// Both the test and pop transaction is done atomically.
pub fn (ch chan) try_pop(obj voidptr) ChanState {
	return .success
}

// try_push returns `ChanState.success` if the object is pushed to the channel.
// try_push effectively both push and test if the transaction `ch <- a` succeeded.
// Both the test and push transaction is done atomically.
pub fn (ch chan) try_push(obj voidptr) ChanState {
	return .success
}

// IError holds information about an error instance.
pub interface IError {
	msg() string
	code() int
}

struct _result {
	is_error bool
	err      IError = none__
	// Data is trailing after err
	// and is not included in here but in the
	// derived Result_xxx types
}

fn _result_ok(data voidptr, mut res _result, size int) {
	unsafe {
		*res = _result{}
		// use err to get the end of ResultBase and then memcpy into it
		vmemcpy(&u8(&res.err) + sizeof(IError), data, size)
	}
}

// str returns the message of IError.
pub fn (err IError) str() string {
	return match err {
		None__ {
			'none'
		}
		Error {
			err.msg()
		}
		MessageError {
			(*err).str()
		}
		else {
			'${err.type_name()}: ${err.msg()}'
		}
	}
}

// Error is the empty default implementation of `IError`.
pub struct Error {}

pub fn (err Error) msg() string {
	return ''
}

pub fn (err Error) code() int {
	return 0
}

// MessageError is the default implementation of the `IError` interface that is returned by the `error()` function.
struct MessageError {
pub:
	msg  string
	code int
}

// str returns both the .msg and .code of MessageError, when .code is != 0 .
pub fn (err MessageError) str() string {
	if err.code > 0 {
		return '${err.msg}; code: ${err.code}'
	}
	return err.msg
}

// msg returns only the message of MessageError.
pub fn (err MessageError) msg() string {
	return err.msg
}

// code returns only the code of MessageError.
pub fn (err MessageError) code() int {
	return err.code
}

@[unsafe]
pub fn (err &MessageError) free() {
	unsafe { err.msg.free() }
}

@[if trace_error ?]
fn trace_error(x string) {
	eprintln('> ${@FN} | ${x}')
}

// error returns a default error instance containing the error given in `message`.
// Example: f := fn (ouch bool) ! { if ouch { return error('an error occurred') } }; f(false)!
@[inline]
pub fn error(message string) IError {
	trace_error(message)
	return &MessageError{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// Example: f := fn (ouch bool) ! { if ouch { return error_with_code('an error occurred', 1) } }; f(false)!
@[inline]
pub fn error_with_code(message string, code int) IError {
	trace_error('${message} | code: ${code}')
	return &MessageError{
		msg:  message
		code: code
	}
}

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

// str for none, returns 'none'
pub fn (_ none) str() string {
	return 'none'
}
