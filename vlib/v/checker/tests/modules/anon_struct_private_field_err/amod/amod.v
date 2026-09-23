module amod

pub struct Outer {
pub mut:
	inner struct {
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
