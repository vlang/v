interface Base {
}

interface Solid {
	Base
}

interface Dense {
	Solid
}

struct Empty {
}

fn greet(x Base) bool {
	return x is Empty
}

fn pass_through(x Solid) Base {
	return x
}

fn test_interface_embedding_implicit_upcast() {
	solid := Solid(Empty{})
	assert greet(Empty{})
	assert greet(solid)
	assert greet(Solid(Empty{}))
	base := pass_through(solid)
	assert base is Empty

	mut base2 := Base(Empty{})
	base2 = solid
	assert base2 is Empty
	assert (solid as Base) is Empty

	dense := Dense(Empty{})
	assert greet(dense)
	base2 = dense
	assert base2 is Empty
	assert (dense as Base) is Empty
}
