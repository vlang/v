// vfmt off
// TODO: without vfmt off, vfmt is buggy, and keeps converting the imports below to `import deprecated_module.bbb.ccc` for some reason, even though the folder has `v.mod`.
// That is unrelated to what this file tests, but should be investigated further and fixed when the module lookup disrepancy is fixed.
import bbb.ccc
import www.ttt
import xxx.yyy
// vfmt on

// Note: www.ttt has been deprecated.
// => compiling this should produce an error,
// showing the deprecation message
fn main() {
	dump(ccc.f())
	dump(ttt.f())
	dump(yyy.f())
	dump(ttt.non_existing)
}
