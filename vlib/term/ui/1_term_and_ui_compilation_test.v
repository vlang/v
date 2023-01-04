import term.ui
import term

fn test_term_and_term_ui_can_compile_together() {
	println('${term.bold('hello')} world ${ui.color_table[0]}')
	assert true
}
