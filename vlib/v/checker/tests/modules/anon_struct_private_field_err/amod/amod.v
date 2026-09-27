module amod

pub struct Base {
	private int
mut:
	counter int
pub:
	public int
}

pub struct Outer {
pub mut:
	inner struct {
		Base
	pub mut:
		value int
		deep  struct {
			hidden int
		pub:
			shown int
		}
	mut:
		secret int
	}
}
