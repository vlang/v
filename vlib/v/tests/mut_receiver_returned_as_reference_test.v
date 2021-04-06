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

// NB: `p` is declared as a `mut` parameter,
// which now only affects its mutability.
fn (mut p Player) set_position(x int, y int) &Player {
	p.x = x
	p.y = y
	// TODO: from the point of view of the V programmer,
	// `p` has still type &Player.
	// assert typeof(p).name == 'Player'
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
