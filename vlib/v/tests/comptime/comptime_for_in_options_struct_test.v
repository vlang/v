struct Options {
	a ?string
	b ?int
	c ?f64
}

fn unwrap_not_none_field_types[T](t T) []string {
	mut arr := []string{}
	$for f in T.fields {
		v := t.$(f.name)
		$if f is $option {
			if v != none {
				arr << typeof(v).name
			}
		}
	}
	return arr
}

fn test_main() {
	arr := unwrap_not_none_field_types(Options{
		a: 'x'
		b: 1
		c: 2.3
	})
	assert arr.join(' ') == 'string int f64'
}
