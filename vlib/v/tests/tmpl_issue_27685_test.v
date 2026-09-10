struct Issue27685Project {
	credits []string
	videos  []string
}

fn issue_27685_shadow_scope_for_template_positions() {
	project := &Issue27685Project{}
	_ := project
	// Keep this source scope long enough to overlap with the generated positions
	// of the template below. Old cgen used file positions for inlined template
	// selectors, so `project.credits` could resolve through this pointer variable.
	// 01
	// 02
	// 03
	// 04
	// 05
	// 06
	// 07
	// 08
	// 09
	// 10
	// 11
	// 12
	// 13
	// 14
	// 15
	// 16
	// 17
	// 18
	// 19
	// 20
	// 21
	// 22
	// 23
	// 24
	// 25
	// 26
	// 27
	// 28
	// 29
	// 30
	// 31
	// 32
	// 33
	// 34
	// 35
	// 36
	// 37
	// 38
	// 39
	// 40
}

fn issue_27685_render_project() string {
	projects := [
		Issue27685Project{
			credits: ['Alice', '', 'Dev']
			videos:  ['Trailer', 'https://example.com']
		},
	]
	mut rendered := ''
	for project in projects {
		rendered = $tmpl('tmpl/template_scope_27685.html')
	}
	return rendered.trim_space()
}

fn test_tmpl_issue_27685_uses_template_scope_for_inherited_selectors() {
	assert issue_27685_render_project() == 'credit Alice (Dev)
video Trailer'
}
