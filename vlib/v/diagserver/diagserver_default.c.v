module diagserver

// serve is a diagnostics server only on Linux, where the child answering a
// request can restart the compiler's worker pools. Elsewhere every run stays a
// one-shot compilation, with no question to answer.
pub fn serve() string {
	return ''
}
