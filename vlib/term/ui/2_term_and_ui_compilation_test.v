import term
import term.ui

fn test_term_and_term_ui_can_compile_together() {
	a := &ui.Context(0)
	println('${term.bold('hello')} world ${int(a)}')
	assert true
}
