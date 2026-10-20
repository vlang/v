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
	mut i := Sum(Int{
		i: 333
	})
	mut s := Sum(String{
		s: 'string'
	})
	assert (i as Int).i == 333
	assert (s as String).s == 'string'
	init(mut i)
	init(mut s)
	assert (i as Int).i == 0
	assert (s as String).s == ''
	if mut i is Int {
		init_int(mut i)
	}
	assert (i as Int).i == 0
}
