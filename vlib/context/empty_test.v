// vtest build: amd64 || arm64
module context

fn test_background() {
	ctx := background()
	assert '&context.Background' == ctx.str()
	if _ := ctx.value('') {
		panic('This should never happen')
	}
}

fn test_todo() {
	ctx := todo()
	assert '&context.TODO' == ctx.str()
	if _ := ctx.value('') {
		panic('This should never happen')
	}
}
