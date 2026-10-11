struct CommentDotsMsg {
	a string
	b []string
}

fn test_field_comment_ending_with_ellipsis_is_not_an_update() {
	m := CommentDotsMsg{
		a: 'x'     // ends with dots...
		b: ['y'] // plain
	}
	assert m.a == 'x'
	assert m.b == ['y']
}

fn test_last_field_comment_ending_with_ellipsis() {
	m := CommentDotsMsg{
		a: 'x'
		b: ['y'] // last field...
	}
	assert m.a == 'x'
	assert m.b == ['y']
}

fn test_update_fields_with_comments_ending_with_ellipsis() {
	base := CommentDotsMsg{
		a: 'base'
		b: ['base']
	}
	m := CommentDotsMsg{
		...base // the base...
		a: 'x' // ends with dots...
	}
	assert m.a == 'x'
	assert m.b == ['base']
}
