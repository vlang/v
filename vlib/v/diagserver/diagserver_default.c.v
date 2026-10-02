module diagserver

// Request is what a child of the diagnostics server was made for; elsewhere
// than on Linux there is no server, and so no request.
pub struct Request {
pub:
	question    string
	from_server bool
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

// shares_checks reports false: without a server, no child answers checks.
pub fn (r &Request) shares_checks() bool {
	return false
}

// asks_for_diagnostics reports false: without a server, no question comes.
pub fn (r &Request) asks_for_diagnostics(question string) bool {
	return false
}

// diagnose_in_grandchild returns true: the check goes on in this process.
pub fn (mut r Request) diagnose_in_grandchild() bool {
	return true
}

// print_diagnostics prints nothing: without a server, the check prints them.
pub fn (mut r Request) print_diagnostics() int {
	return 0
}

// print_partial_with does nothing: without a server, the check prints all its
// diagnostics at once.
pub fn (mut r Request) print_partial_with(print fn () int) {}

// incremental_record returns '': without a server, no check left one.
pub fn (r &Request) incremental_record() string {
	return ''
}

// keep_incremental_record does nothing without a server.
pub fn (r &Request) keep_incremental_record(text string) {}

// keep_busy_with does nothing without a server: no question comes.
pub fn (mut r Request) keep_busy_with(step fn () bool) {}
