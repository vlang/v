interface Bus {
	name() string
}

struct Silent {
mut:
	verbatim bool
}

fn (s Silent) name() string {
	return 'silent ${s.verbatim}'
}

fn return_mut_param(mut b Bus) Bus {
	return b
}

fn return_after_smartcast(mut b Bus) Bus {
	if mut b is Silent {
		b.verbatim = true
	}
	return b
}

fn return_inside_smartcast(mut b Bus) Bus {
	if mut b is Silent {
		b.verbatim = true
		return b
	}
	return b
}

fn return_mut_param_with_defer(mut b Bus) Bus {
	defer {
		b.name()
	}
	if mut b is Silent {
		b.verbatim = true
	}
	return b
}

fn test_return_mut_interface_param() {
	mut b := Bus(Silent{})
	assert return_mut_param(mut b).name() == 'silent false'
}

fn test_return_mut_interface_param_after_smartcast() {
	mut b := Bus(Silent{})
	assert return_after_smartcast(mut b).name() == 'silent true'
	assert b.name() == 'silent true'
}

fn test_return_mut_interface_param_inside_smartcast() {
	mut b := Bus(Silent{})
	assert return_inside_smartcast(mut b).name() == 'silent true'
}

fn test_return_mut_interface_param_with_defer() {
	mut b := Bus(Silent{})
	assert return_mut_param_with_defer(mut b).name() == 'silent true'
}
