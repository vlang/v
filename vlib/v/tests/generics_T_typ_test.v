import math

struct Any {
mut:
	data voidptr
	typ  int
}

fn make_any<T>(obj T) Any {
	tsize := int(sizeof(T))
	mut a := Any{
		typ: T.typ
		data: unsafe { malloc(tsize) }
	}
	unsafe {
		vmemcpy(a.data, &obj, tsize)
	}
	return a
}

fn cast<T>(obj Any) ?T {
	if T.typ == obj.typ {
		return *&T(obj.data)
	}
	return none
}

fn test_any_values() {
	arr := [make_any(true), make_any(false), make_any(7), make_any('cat'),
		make_any([3.1415926535])]
	for elm in arr {
		if b := cast<bool>(elm) {
			println(!b)
		} else if i := cast<int>(elm) {
			println(i + 1)
		} else if s := cast<string>(elm) {
			println(s + '!')
		} else if f := cast<[]f64>(elm) {
			println(f[0])
		}
	}
	if b := cast<bool>(arr[0]) {
		assert b == true
	}
	if b := cast<bool>(arr[1]) {
		assert b == false
	}
	if s := cast<string>(arr[3]) {
		assert s == 'cat'
	}
	if f := cast<[]f64>(arr[4]) {
		assert math.veryclose(f[0], 3.1415926535)
	}
}
