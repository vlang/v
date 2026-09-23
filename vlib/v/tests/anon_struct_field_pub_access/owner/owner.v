module owner

pub struct Meta {
	secret int
pub mut:
	id int
}

pub struct Outer {
pub mut:
	inner struct {
		Meta
	pub mut:
		value int
		deep  struct {
		pub mut:
			x int
		}
	}
pub:
	info struct {
	pub:
		name string = 'outer'
	}
}
