type TestSum = int | string

fn gen[T](val T) {
	$if val is $sumtype {
		$for f in T.variants {
			dump(f)
			dump(f.typ)
			$if f.typ is $int {
				dump('is int')
				assert f.typ == typeof[int]().idx
			} $else $if f.typ is string {
				dump('is string')
				assert f.typ == typeof[string]().idx
			}
		}
	}
}

fn test_main() {
	a := TestSum(123)
	gen(a)
}
