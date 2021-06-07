struct Empty {}

type SumType = Empty | int

fn isok(a SumType, b SumType) bool {
	return !(match a {
		int {
			match b {
				int { a == b }
				Empty { false }
			}
		}
		Empty {
			false
		}
	} || match a {
		int {
			match b {
				int { a + 10 == b - 10 }
				Empty { false }
			}
		}
		Empty {
			false
		}
	})
}

fn test_match_with_multi_sumtype_exprs() {
	a := SumType(1)
	b := SumType(Empty{})

	res := isok(a, b)

	dump(res)
	assert res
}
