// vtest vflags: -gc boehm
// vtest build: !race?

fn test_gc_warn_proc_roundtrip() {
	gc_set_warn_proc(gc_get_warn_proc())
}
