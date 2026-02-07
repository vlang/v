// BEAM backend option/result types
// V's error handling types for BEAM backend
module builtin

// IError holds information about an error instance.
pub interface IError {
	msg() string
	code() int
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

// str returns both the .msg and .code of MessageError, when .code is != 0.
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

// str returns the message of IError.
pub fn (err IError) str() string {
	return err.msg()
}

// error returns a default error instance containing the error given in `message`.
// Example: f := fn (ouch bool) ! { if ouch { return error('an error occurred') } }; f(false)!
@[inline]
pub fn error(message string) IError {
	return &MessageError{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// Example: f := fn (ouch bool) ! { if ouch { return error_with_code('an error occurred', 1) } }; f(false)!
@[inline]
pub fn error_with_code(message string, code int) IError {
	return &MessageError{
		msg:  message
		code: code
	}
}

// None__ is the internal representation of none
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

// ChanState describes the result of an attempted channel transaction.
pub enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

// close closes the channel for further push transactions.
// On BEAM: channels are implemented via vbeam_concurrency
pub fn (ch chan) close() {}

// try_pop returns `ChanState.success` if an object is popped from the channel.
pub fn (ch chan) try_pop(obj voidptr) ChanState {
	return .success
}

// try_push returns `ChanState.success` if the object is pushed to the channel.
pub fn (ch chan) try_push(obj voidptr) ChanState {
	return .success
}

// Internal V option/result types for BEAM
struct _result {
	is_error bool
	err      IError = none__
}

struct _option {
	state u8
	err   IError = none__
}

const none__ = IError(&None__{})
