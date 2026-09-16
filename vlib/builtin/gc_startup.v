module builtin

// gc_runtime_init is called by the platform builtin initializer when a collector
// is selected. V1 already initializes its collector from the legacy C generator;
// only V3 needs the builtin startup path.
@[inline]
fn gc_runtime_init() {
	$if v3_backend ? {
		$if gcboehm ? {
			v3_gcboehm_runtime_init()
		}
		$if vgc ? {
			v3_vgc_runtime_init()
		}
	}
}
