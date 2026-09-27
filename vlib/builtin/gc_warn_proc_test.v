// vtest vflags: -gc boehm

fn test_gc_warn_proc_roundtrip() {
	gc_set_warn_proc(gc_get_warn_proc())
}
