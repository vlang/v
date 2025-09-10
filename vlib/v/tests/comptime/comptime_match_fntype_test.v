type FN1 = fn (a int) int

type FN2 = fn (a int, b int) int

type FN3 = FN1 | FN2

fn invoke[T](cb T) int {
	$match T {
		FN1 {
			return cb(1)
		}
		FN2 {
			return cb(1, 2)
		}
		$else {
			return 0
		}
	}
}

fn test_main() {
	ret1 := invoke(fn (a int) int {
		return a
	})
	assert ret1 == 1

	ret2 := invoke(fn (a int, b int) int {
		return a + b
	})
	assert ret2 == 3
}
