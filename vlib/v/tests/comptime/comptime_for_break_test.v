struct Test {
	a string
	b string
}

fn test_for() {
	$for field in Test.fields {
		for attr in field.attrs {
			break
		}
	}
	$for field in Test.fields {
		for attr in field.attrs {
			continue
		}
	}
}
