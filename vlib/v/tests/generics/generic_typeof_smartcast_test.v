// `typeof` in a generic body names the smartcast type, as it does in the
// equivalent non-generic body.
type IntOrStr = int | string

interface Named {
	name string
}

struct Dog {
	name string
}

struct Point {
mut:
	x int
}

fn option_names[T](a ?T) []string {
	mut names := []string{}
	if a != none {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	if a == none {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn mut_option_names[T](mut a ?T) []string {
	mut names := []string{}
	if a != none {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn plain_option_names(a ?Point) []string {
	mut names := []string{}
	if a != none {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	if a == none {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn plain_mut_option_names(mut a ?Point) []string {
	mut names := []string{}
	if a != none {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn variant_names[T](a T) []string {
	mut names := []string{}
	if a is int {
		names << typeof(a).name
	}
	if a !is int {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn plain_variant_names(a IntOrStr) []string {
	mut names := []string{}
	if a is int {
		names << typeof(a).name
	}
	if a !is int {
		names << typeof(a).name
	} else {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn interface_names[T](a T) []string {
	mut names := []string{}
	if a is Dog {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn plain_interface_names(a Named) []string {
	mut names := []string{}
	if a is Dog {
		names << typeof(a).name
	}
	names << typeof(a).name
	return names
}

fn match_names[T](a T) []string {
	mut names := []string{}
	match a {
		int {
			names << typeof(a).name
		}
		string {
			names << typeof(a).name
		}
	}
	names << typeof(a).name
	return names
}

fn plain_match_names(a IntOrStr) []string {
	mut names := []string{}
	match a {
		int {
			names << typeof(a).name
		}
		string {
			names << typeof(a).name
		}
	}
	names << typeof(a).name
	return names
}

fn test_option_smartcasts() {
	mut o := ?Point(Point{
		x: 1
	})
	assert plain_option_names(o) == ['Point', 'Point', '?Point']
	assert option_names(o) == plain_option_names(o)
	assert plain_mut_option_names(mut o) == ['&Point', '?&Point']
	assert mut_option_names(mut o) == plain_mut_option_names(mut o)
	o = none
	assert option_names(o) == plain_option_names(o)
	mut ai := ?[]int([1, 2])
	assert option_names(ai) == ['[]int', '[]int', '?[]int']
	assert mut_option_names(mut ai) == ['&[]int', '?&[]int']
}

fn test_sum_type_and_interface_smartcasts() {
	assert plain_variant_names(IntOrStr(3)) == ['int', 'int', 'IntOrStr']
	assert variant_names(IntOrStr(3)) == plain_variant_names(IntOrStr(3))
	assert plain_variant_names(IntOrStr('s')) == ['IntOrStr', 'IntOrStr']
	assert variant_names(IntOrStr('s')) == plain_variant_names(IntOrStr('s'))
	assert plain_match_names(IntOrStr(3)) == ['int', 'IntOrStr']
	assert match_names(IntOrStr(3)) == plain_match_names(IntOrStr(3))
	assert match_names(IntOrStr('s')) == plain_match_names(IntOrStr('s'))
	assert plain_interface_names(Named(Dog{'x'})) == ['&Dog', 'Named']
	assert interface_names(Named(Dog{'x'})) == plain_interface_names(Named(Dog{'x'}))
}
