interface Any {}

struct Concrete {
	a int
}

struct Container {
	concrete Any
}

fn (container &Container) get_first_struct<T>() ?&T {
	concrete := container.concrete
	if concrete is T {
		println(concrete.a)
		return concrete
	}
	return error("can't cast")
}

fn test_generic_empty_interface_to_struct() {
	concrete := Concrete{12345}
	container := Container{concrete}
	cast_concrete := container.get_first_struct<Concrete>() or { &Concrete{} }
	assert 12345 == cast_concrete.a
}
