type Arr = []int | []string

fn test_match_with_array_map_in_branches() {
	arr := Arr([0, 1])
	ret := match arr {
		[]int {
			arr.map(fn (s int) string {
				return s.str()
			}).str()
		}
		else {
			''
		}
	}
	println(ret)
	assert ret == "['0', '1']"
}

fn test_match_expr_of_multi_expr_stmts() {
	a := 1
	ret := match a {
		1 {
			mut m := map[string]int{}
			m['two'] = 2
			m['two']
		}
		else {
			int(0)
		}
	}
	println(ret)
	assert ret == 2
}

fn test_match_expression_on_complex_bool_conditions() {
	s := 'hello'
	x := match true {
		s[1] == `e` { 'first' }
		(s[1] == `e`) { 'second' }
		else { 'not found' }
	}
	println(x)
	assert x == 'first'
}
