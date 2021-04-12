module context

fn test_background() {
	ctx := background()
	assert 'context.Background' == ctx.str()
	if _ := ctx.value('') {
		println('This should not happen')
		assert false
	}
}

fn test_todo() {
	ctx := todo()
	assert 'context.TODO' == ctx.str()
	if _ := ctx.value('') {
		println('This should not happen')
		assert false
	}
}
