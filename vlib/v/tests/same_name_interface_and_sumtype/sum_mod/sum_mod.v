module sum_mod

// Any is the namesake sum type used by the regression fixture.
pub type Any = int | string

// describe reports which Any variant it receives.
pub fn describe(a Any) string {
	return match a {
		int { 'int' }
		string { 'string' }
	}
}
