enum Kind {
	one
	two
}

fn bounds() (int, int) { return 3, 4 }

fn pick(k Kind) (int, int) {
	start, end := match k {
		.one {
			n := 1
			n, 2
		}
		.two { bounds() }
	}
	return start, end
}

fn test_match_tuple_call() {
	a, b := pick(.one)
	c, d := pick(.two)
	assert [a, b, c, d] == [1, 2, 3, 4]
}

fn pick_call_first(k Kind) (int, int) {
	return match k {
		.one { bounds() }
		.two { 5, 6 }
	}
}

fn labelled_bounds() (int, string) {
	return 7, 'seven'
}

fn pick_label(k Kind) (int, string) {
	return match k {
		.one { 8, 'eight' }
		.two { labelled_bounds() }
	}
}

fn test_match_tuple_call_order_and_element_types() {
	a, b := pick_call_first(.one)
	c, d := pick_call_first(.two)
	assert [a, b, c, d] == [3, 4, 5, 6]
	n, text := pick_label(.one)
	other_n, other_text := pick_label(.two)
	assert n == 8 && text == 'eight'
	assert other_n == 7 && other_text == 'seven'
}
