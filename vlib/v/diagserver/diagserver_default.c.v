module diagserver

// Request is what a child of the diagnostics server was made for; elsewhere
// than on Linux there is no server, and so no request.
pub struct Request {
pub:
	question string
}

// serve is a diagnostics server only on Linux, where the child answering a
// request can restart the compiler's worker pools. Elsewhere every run stays a
// one-shot compilation, with no question to answer.
pub fn serve() Request {
	return Request{}
}

// answers_again reports whether this process can answer more questions after
// its first ones: never, without a server.
pub fn (r &Request) answers_again() bool {
	return false
}

// keep_inputs does nothing without a server.
pub fn (mut r Request) keep_inputs(digests map[string]string, imports_hold fn () bool) {}

// next_question returns none: without a server, no question comes.
pub fn (mut r Request) next_question(code int) ?string {
	return none
}
