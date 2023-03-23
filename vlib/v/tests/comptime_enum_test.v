enum Test {
	foo
	bar
}

enum Test2 {
	alpha
	bravo
	charlie
}

fn test_main() {
	$for item in Test.values {
		assert item.name in ['foo', 'bar']
		match item.value {
			.foo {
				println('foo>> item: ${item.name}')
				assert item.value == .foo
			}
			.bar {
				println('foo>> item: ${item.name}')
				assert item.value == .bar
			}
		}
		if item.value == .foo {
			println('foo>> item: ${item.name}')
			assert item.value == .foo
		} else if item.value == .bar {
			println('foo>> item: ${item.name}')
			assert item.value == .bar
		}
	}
}

fn test_selectors() {
	$for value in Test2.values {
		println(value.name)
		assert dump(value.value) == value.value
	}
}
