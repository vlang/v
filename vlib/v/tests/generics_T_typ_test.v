import math

struct Any {
mut:
	data voidptr
	typ  int
}

fn (mut a Any) set_typ(typ int) bool {
	a.typ = typ
	return true
}

fn make_any<T>(obj T) Any {
	mut a := Any{}
	a.set_typ(T.typ)
	unsafe {
		data := malloc(int(sizeof(T)))
		vmemcpy(data, &obj, int(sizeof(T)))
		a.data = data
	}
	return a
}

fn cast<T>(obj Any) ?T {
	if int(T) == obj.typ {
		return *&T(obj.data)
	}
	return none
}

fn test_any_values() {
	arr := [make_any(true), make_any(false), make_any(7), make_any('cat'),
		make_any([3.1415926535]),
	]
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
