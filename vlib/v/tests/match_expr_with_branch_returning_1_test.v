type Arr = []int | []string

fn (arr Arr) str() string {
	return match arr {
		[]string {
			arr.join(' ')
		}
		[]int {
			return arr.str()
		}
	}
}

fn test_match_expr_with_branch_returning() {
	println(Arr([0, 0]))
	assert '${Arr([0, 0])}' == '[0, 0]'
	println(Arr(['0', '0']))
	assert '${Arr(['0', '0'])}' == '0 0'
}
