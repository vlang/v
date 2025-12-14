module value

pub interface Value {
	str() string
}

pub type List = []Value

pub fn (x List) str() string {
	return x.str()
}
