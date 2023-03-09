enum Test {
	foo
	bar
}

fn test_main() {
	$for item in Test.vals {
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
