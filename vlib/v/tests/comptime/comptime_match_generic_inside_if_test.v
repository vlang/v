fn strange[T](a T, b T) T {
	if a != 0 {
		$match T {
			u8 {
				return a
			}
			i8 {
				if a > 0 {
					return a
				} else {
					return b
				}
			}
			$else {
				$compile_error('unknown')
			}
		}
	} else {
		return b
	}
}

fn test_main() {
	assert strange[u8](0, 1) == 1
	assert strange[i8](0, 1) == 1
}
