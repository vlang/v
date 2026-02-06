fn merged[K, V](a map[K]V, b map[K]V) map[K]V {
	mut o := a.clone()
	for k, v in b {
		$if V is $map {
			o[k] = merged(o[k], v)
		} $else {
			o[k] = v
		}
	}
	return o
}

fn test_main() {
	a := {
		'aa': {
			'11': 1
		}
	}
	b := {
		'bb': {
			'22': 2
		}
	}
	c := merged(a, b)
	assert c == {
		'aa': {
			'11': 1
		}
		'bb': {
			'22': 2
		}
	}
}
