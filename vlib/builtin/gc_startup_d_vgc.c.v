module builtin

// Keep VGC startup behind the same hook as Boehm so platform builtin_init can
// initialize whichever collector the V3 driver selected.
fn gc_runtime_init() {
	vgc_init()
}
