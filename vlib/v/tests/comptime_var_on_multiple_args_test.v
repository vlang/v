module main

fn ok3[T](o T) T {
	return o
}

fn ok2[T](o T, s string) string {
	return s
}

fn ok[T](o T, s string) string {
	return '${o}${s}'
}

struct Test {
	s string
}

fn test_main() {
	s := Test{'foo'}

	$for f in Test.fields {
		assert ok3(s.$(f.name)).str() == 'foo'
	}
	$for f in Test.fields {
		assert ok(s.$(f.name), 'bar') == 'foobar'
	}
	$for f in Test.fields {
		assert ok2(s.$(f.name), 'ok') == 'ok'
	}
}
