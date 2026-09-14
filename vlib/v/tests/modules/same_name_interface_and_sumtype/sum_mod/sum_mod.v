module sum_mod

pub type Any = int | string

pub fn describe(a Any) string {
	return match a {
		int { 'int' }
		string { 'string' }
	}
}
