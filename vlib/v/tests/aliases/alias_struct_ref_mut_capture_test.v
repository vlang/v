struct App {
mut:
	score int
}

type AppRef = &App

fn make_handler(mut app AppRef) fn () {
	return fn [mut app] () {
		app.score++
	}
}

fn test_main() {
	mut a := App{}
	h := make_handler(mut &a)
	assert a.score == 0
	h()
	assert a.score == 1
}
