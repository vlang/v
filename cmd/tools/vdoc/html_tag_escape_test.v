module main

fn test_html_tag_escape() {
	assert html_tag_escape('<abc>') == '&lt;abc&gt;'
	assert html_tag_escape('`<abc>`') == '`<abc>`'
}
