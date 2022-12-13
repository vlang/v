import encoding.html

fn test_escape_html() {
	assert html.escape(input: '<>&') == '&lt;&gt;&amp;'
	assert html.escape(input: 'No change') == 'No change'
	assert html.escape(input: '<b>Bold text</b>') == '&lt;b&gt;Bold text&lt;/b&gt;'
	assert html.escape(input: '<img />') == '&lt;img /&gt;'
	assert html.escape(input: "' onmouseover='alert(1)'") == '&#x27; onmouseover=&#x27;alert(1)&#x27;'
	assert html.escape(input: "<a href='http://www.example.com'>link</a>") == '&lt;a href=&#x27;http://www.example.com&#x27;&gt;link&lt;/a&gt;'
	assert html.escape(input: "<script>alert('hello');</script>") == '&lt;script&gt;alert(&#x27;hello&#x27;);&lt;/script&gt;'
}
