module picoev

@[if picoev_verbose_errors ?]
fn elog(msg string) {
	eprintln(msg)
}
