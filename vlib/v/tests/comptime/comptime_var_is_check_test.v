type TestSum = int | string

fn gen[T, R](sum T) R {
	$if T is $sumtype {
		$for v in sum.variants {
			if sum is v {
				$if sum is R {
					return sum
				}
			}
		}
	}
	return R{}
}

fn test_main() {
	assert dump(gen[TestSum, string](TestSum('foo'))) == 'foo'
	assert dump(gen[TestSum, int](TestSum(123))) == 123
}
