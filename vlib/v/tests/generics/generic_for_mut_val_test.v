struct ValueMenu {
	name string
}

fn (mut v ValueMenu) resp(resp []u8) {
	println('menu ${v.name} ${resp}')
}

fn value_resp[T](mut t T, r []u8) {
	$if T is $array {
		value_array_resp(mut t, r)
	}
}

fn value_array_resp[T](mut t T, r []u8) {
	$if T is []ValueMenu {
		for _, mut i in t { // mut i -> C error
			i.resp(r)
		}
		for i := 0; i < t.len; i++ { // WORKAROUND
			t[i].resp(r)
		}
	}
}

struct Colors {
mut:
	normal []ValueMenu = [ValueMenu{'blink'}, ValueMenu{'hue'}]
}

fn (c &Colors) resp(resp []u8) {
	$for f in c.fields {
		mut m := c.$(f.name)
		value_resp(mut m, resp)
	}
}

fn test_main() {
	colors := &Colors{}
	colors.resp([u8(0), 1, 2])
	assert true
}
