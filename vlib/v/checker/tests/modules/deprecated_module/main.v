import bbb.ccc
import www.ttt
import xxx.yyy

// Note: www.ttt has been deprecated.
// => compiling this should produce an error,
// showing the deprecation message
fn main() {
	dump(ccc.f())
	dump(ttt.f())
	dump(yyy.f())
	dump(ttt.non_existing)
}
