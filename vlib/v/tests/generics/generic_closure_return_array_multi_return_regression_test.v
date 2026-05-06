type Issue17876Fn[I, O, R] = fn (I) !(O, R)

fn issue17876_chain[I, O, R](funcs ...Issue17876Fn[I, O, R]) Issue17876Fn[I, []O, R] {
	return fn [funcs] [I, O, R](input I) !([]O, R) {
		mut last_input := input
		mut results := []O{}

		for function in funcs {
			got, remain := function(last_input)!
			results << got
			last_input = remain
		}

		return results, last_input
	}
}

fn issue17876_take_first(input string) !([]string, string) {
	if input.len == 0 {
		return []string{}, input
	}
	return [input[..1]], input[1..]
}

fn test_issue_17876_generic_closure_return_array_multi_return() ! {
	results, remain := issue17876_chain[string, []string, string]()('STRING')!
	assert results == [][]string{}
	assert remain == 'STRING'

	results2, remain2 :=
		issue17876_chain[string, []string, string](issue17876_take_first)('STRING')!
	assert results2 == [['S']]
	assert remain2 == 'TRING'
}
