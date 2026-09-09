module b

pub fn take(x &C.dupe) int {
	return x.new
}

@[typedef]
pub struct C.dupe {
	new int
}

pub type Dupe = C.dupe

pub fn make() &C.dupe {
	return &C.dupe{
		new: 1
	}
}
