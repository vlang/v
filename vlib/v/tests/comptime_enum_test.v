enum Test {
	foo
	bar
}

fn test_print_value_name() {
	$for value in Test.values {
		println(value.name)
	}
}

fn test_print_value_value() {
	$for value in Test.values {
		println(value.value)
	}
}

fn test_print_both() {
	$for values in Test.values {
		println(values.name)
		println(values.value)
	}
}

fn test_comptime_for_in_enum_values() {
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
