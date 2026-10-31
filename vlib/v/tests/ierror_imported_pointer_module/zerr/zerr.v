module zerr

pub struct MyError {
	Error
pub:
	n int
}

// make returns an imported concrete error boxed as IError.
pub fn make(n int) IError {
	return MyError{ n: n }
}
