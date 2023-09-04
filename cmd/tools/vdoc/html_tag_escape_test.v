module main

fn test_html_tag_escape() {
	assert html_tag_escape('assert <abc> 123') == 'assert &lt;abc&gt; 123'
	assert html_tag_escape('`assert <abc> 123`') == '`assert <abc> 123`'
}
