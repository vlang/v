module wrapper

struct C.Hidden {
	value int
}

pub fn leak() C.Hidden {
	return C.Hidden{
		value: 1
	}
}
