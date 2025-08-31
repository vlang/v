fn check_unaliased[T](t T) bool {
	$match T.unaliased_typ {
		int {
			return true
		}
		$else {
			return false
		}
	}
}

fn check_typ[T](t T) bool {
	$match T.typ {
		int {
			return true
		}
		$else {
			return false
		}
	}
}

type FooInt = int

fn test_main() {
	assert check_unaliased(1)
	assert !check_unaliased('')
	assert !check_unaliased(1.2)
	assert check_unaliased(FooInt(0))

	assert check_typ(1)
	assert !check_typ('')
	assert !check_typ(1.2)
	assert !check_typ(FooInt(0))
}
