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

fn test_interface_embedding_implicit_upcast() {
	solid := Solid(Empty{})
	assert greet(solid)

	mut base := Base(Empty{})
	base = solid
	assert base is Empty
	assert (solid as Base) is Empty

	dense := Dense(Empty{})
	assert greet(dense)
	base = dense
	assert base is Empty
	assert (dense as Base) is Empty
}
