module amod

fn foo() int {
	return 2
}

pub fn value() int {
	$if never_defined ? {
		return foo()
	}
	return 3
}
