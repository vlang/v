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
				arr << '${typeof(v).name}:${f.name}=`${v}`'

				w := v // assign

				$if w is string {
					t_string(w) // fn call with string value
				}

				t_generic(w) // fn call with generic value
			}
		}
	}
	return arr
}

fn t_string(s string) {
	assert s == 'x'
}

fn t_generic[T](t T) {
	$if t is int {
		assert t == 1
		return
	}
	$if t is f64 {
		assert t == 2.3
		return
	}
	$if t is string {
		t_string(t)
		return
	}
	assert false
}

fn test_main() {
	arr := unwrap_not_none_field_types(Options{
		a: 'x'
		b: 1
		c: 2.3
	})
	assert arr.join(' ') == 'string:a=`x` int:b=`1` f64:c=`2.3`'
}
