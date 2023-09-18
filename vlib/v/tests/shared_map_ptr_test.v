struct Abc {
	f shared map[string]&Abc
}

fn test_main() {
	c := Abc{}
	b := Abc{}
	a := Abc{
		f: b.f
	}

	lock a.f, b.f {
		a.f['a'] = &c
	}
	lock b.f {
		b.f['b'] = &c
	}

	dump(b.f)
	rlock b.f {
		assert b.f.len == 2
	}
}
