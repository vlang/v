module builtin

// gc_runtime_init is only compiled into V3 programs. V1 already initializes its
// collector from the legacy C generator and must not see the V3 startup helpers.
@[inline]
fn gc_runtime_init() {
	$if gcboehm ? {
		v3_gcboehm_runtime_init()
	}
	$if vgc ? {
		v3_vgc_runtime_init()
	}
}
