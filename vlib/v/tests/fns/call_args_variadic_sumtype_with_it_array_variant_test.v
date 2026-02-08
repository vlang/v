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
	println(arr)
}

fn print_int_any(_ int, arr ...Any) {
	print_any(...arr)
}

fn test_main() {
	print_any(1, Any(1), [Any(1)])
	print_any(1)
	print_any(Any(1))
	print_any([Any(1)])
	print_any([Any(1)], 1)
	print_int_any(1, 1)
	print_int_any(1, [Any(1)])
}
