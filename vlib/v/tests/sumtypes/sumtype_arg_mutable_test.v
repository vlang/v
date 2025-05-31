struct Int {
mut:
	i int
}

struct String {
mut:
	s string
}

type Sum = Int | String

fn init(mut s Sum) {
	match mut s {
		Int { s.i = 0 }
		String { s.s = '' }
	}
}

fn init_int(mut i Int) {
	i.i = 0
}

fn test_main() {
	mut i := Int{
		i: 333
	}
	mut s := String{
		s: 'string'
	}
	if $d('mutable_sumtype', false) {
		assert i.i == 333
		assert s.s == 'string'
		init(mut i)
		init(mut s)
		assert i.i == 0
		assert s.s == ''
		init_int(mut i)
		assert i.i == 0
	}
}
