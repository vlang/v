// Add more examples of potentially buggy patterns in vlib/v/parser/templates/index.html
fn test_tmpl_comptime() {
	index := $tmpl('templates/index.html').trim_space()
	// dump(index)
	assert index.contains('<br>Line ending with percent %\n')
	assert index.contains('<br>Line ending with at $\n')
	assert index.contains('<br>Line ending with ampersand &\n')
	assert index.contains('<br>Line ending with hash #\n')
	assert index.contains('<br>Line ending with slash /\n')
	assert index.contains('<br>Line ending with dollar $\n')
	assert index.contains('<br>Line ending with caret ^\n')
}

// Add more tests

// File contents for building repsonse
const base = '<p>This is the base file</p>'
const child = '<p>This is the child file</p>'
const grandchild = '<p>This is the grandchild file</p>'
const parent = '<p>This is the parent file</p>'

// Call the parent file which contains all child templates
fn test_tmpl_include_parent() {
	expected := [base, base, base, child, base, base, base, child, grandchild, parent].join('\n')
	parent_tmpl := $tmpl('templates/parent.html')
	assert parent_tmpl.contains(expected)
}

// Test the child template which calls parent template
fn test_tmpl_include_child() {
	expected := [base, base, child].join('\n')
	child_tmpl := $tmpl('templates/nested/child.html')
	assert child_tmpl.contains(expected)
}

// Test the grandchild templates calling both parent and grandparent templates
fn test_tmpl_include_grandchild() {
	expected := [base, base, base, child, grandchild].join('\n')
	child_tmpl := $tmpl('templates/nested/nested_deeper/grandchild.html')
	assert child_tmpl.contains(expected)
}
