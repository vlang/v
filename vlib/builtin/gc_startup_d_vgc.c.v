module builtin

// Keep VGC startup behind a V3-only hook so existing V1 startup ordering is unchanged.
fn v3_vgc_runtime_init() {
	vgc_init()
}
