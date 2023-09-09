module main

fn test_html_tag_escape() {
	assert html_tag_escape('abc <b>bold</b> 123') == 'abc &lt;b&gt;bold&lt;/b&gt; 123'
	assert html_tag_escape('`abc <b>bold</b> 123`') == '`abc <b>bold</b> 123`'
}
