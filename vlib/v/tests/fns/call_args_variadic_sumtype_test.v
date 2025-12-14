type Animal = Dog | Cat

struct Dog {
	name string
}

struct Cat {
	name string
}

fn print_names(animals ...Animal) {
	for animal in animals {
		assert animal.name == 'Kitty'
	}
}

type Any = []Any | int

fn print_any(arr ...Any) {
	if any := arr[0] {
		match any {
			int {
				assert any == 1
			}
			[]Any {
				assert any == [Any(1)]
			}
		}
	}
	assert true
}

fn print_int_any(_ int, arr ...Any) {
	print_any(...arr)
}

fn test_main() {
	cat := Cat{
		name: 'Kitty'
	}
	mut animals := []Animal{}
	animals << cat
	print_names(...animals)

	print_any(1, Any(1), [Any(1)])
	print_any(1)
	print_any(Any(1))
	print_any([Any(1)])
	print_any([Any(1)], 1)
	print_int_any(1, 1)
	print_int_any(1, [Any(1)])
}
