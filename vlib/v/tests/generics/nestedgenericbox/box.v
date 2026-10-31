module nestedgenericbox

import nestedgenericseed { default_number }

pub struct Box[T] {
pub:
	value T = T(default_number())
}

pub struct Outer {
pub:
	box Box[int]
}
