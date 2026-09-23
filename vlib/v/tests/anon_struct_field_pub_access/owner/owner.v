module owner

pub struct Outer {
pub mut:
	inner struct {
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
