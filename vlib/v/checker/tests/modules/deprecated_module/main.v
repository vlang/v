import deprecated_module.bbb.ccc
import deprecated_module.www.ttt
import deprecated_module.xxx.yyy

// NB: www.ttt has been deprecated.
// => compiling this should produce an error,
// showing the deprecation message
fn main() {
	dump(ccc.f())
	dump(ttt.f())
	dump(yyy.f())
	dump(ttt.non_existing)
}
