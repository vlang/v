module context

const none_ = IError(&None{})

struct None {
	msg  string
	code int
}

fn (_ None) str() string {
	return 'none'
}
