struct Player {
mut:
	name string
	x    int
	y    int
}

fn (mut p Player) set_name(name string) &Player {
	p.name = name
	return p // because of automatic (de)reference of return values
}

fn (mut p Player) set_position(x int, y int) &Player {
	p.x = x
	p.y = y
	// NB: `p` is declared as a `mut` parameter,
	// returning &p here just ignores `&` for now.
	// TODO: this should be a checker error.
	// `&p` should be of type `&&Player`
	return &p
}

fn test_mut_receiver() {
	mut p := &Player{}
	f := p.set_name('frodo')
	assert u64(p) == u64(f)
	z := p.set_position(111, 222)
	assert u64(p) == u64(z)
	//
	assert p.name == 'frodo'
	assert p.x == 111
	assert p.y == 222
	assert u64(p.set_name('bilbo')) == u64(p)
	assert p.name == 'bilbo'
}
