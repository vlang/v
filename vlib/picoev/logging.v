module picoev

@[if picoev_verbose_errors ?]
fn elog(msg string) {
	eprintln(msg)
}

@[if trace_fd ?]
fn trace_fd(msg string) {
	eprintln(msg)
}
