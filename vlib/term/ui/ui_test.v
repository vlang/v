import term.ui

// This test just ensures that programs importing term.ui can compile

fn test_a_simple_term_ui_program_can_be_compiled() {
	println(ui.color_table)
	assert true
}
