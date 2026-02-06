fn unwrap(a ?[3]u8) {
	if a == none {
		println('${@FN}:${@LINE}: ${typeof(a).name}')
		assert typeof(a).name == '?[3]u8'
	} else {
		println('${@FN}:${@LINE}: ${typeof(a).name}')
		assert typeof(a).name == '[3]u8'
	}
	if a != none {
		println('${@FN}:${@LINE}: ${typeof(a).name}')
		assert typeof(a).name == '[3]u8'
	} else {
		println('${@FN}:${@LINE}: ${typeof(a).name}')
		assert typeof(a).name == '?[3]u8'
	}
}

struct Opt {
pub mut:
	f ?[3]u8
}

fn unwrap_field(opt Opt) {
	if opt.f == none {
		println('${@FN}:${@LINE}: ${typeof(opt.f).name}')
		assert opt.f == ?[3]u8(none)
		assert typeof(opt.f).name == '?[3]u8'
	} else {
		println('${@FN}:${@LINE}: ${typeof(opt.f).name}')
		assert opt.f == [u8(2), 2, 2]!
		assert typeof(opt.f).name == '[3]u8'
	}
	if opt.f != none {
		println('${@FN}:${@LINE}: ${typeof(opt.f).name}')
		assert opt.f == [u8(2), 2, 2]!
		assert typeof(opt.f).name == '[3]u8'
	} else {
		println('${@FN}:${@LINE}: ${typeof(opt.f).name}')
		assert opt.f == ?[3]u8(none)
		assert typeof(opt.f).name == '?[3]u8'
	}
}

fn test_main() {
	mut a := ?[3]u8(none)
	unwrap(a)
	a = [3]u8{init: 1}
	unwrap(a)

	mut f := Opt{}
	unwrap_field(f)
	f.f = [3]u8{init: 2}
	unwrap_field(f)
}
