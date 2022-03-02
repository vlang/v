fn test_comptime_generic() {
	a := [5]int{}
	func1(&a)
}

[inline]
pub fn func1<T>(t &T) {
	func2<T>(t)
}

[inline]
pub fn func2<T>(t &T) {
	$if T is $Array {
		unsafe {
			for i in 0 .. t.len {
				func2(&t[i])
			}
		}
	} $else $if T is $Map {
		// fake_map(t, df)
	} $else $if T is $Struct {
		$for f in T.fields {
			$if f.typ is string {
			}
			// Dummy expression to generate and specify t.$(f.name)'s type

			// mut attrs := get_attrs(t.$(f.name), f)

			// if !attrs.skip() {
			// 	fake_data_wdf(&(t.$(f.name)))
			// }
		}
	} $else {
		unsafe {
			// *t = fake_primitive_value<T>(df) or { panic(err) }
		}
	}
}
