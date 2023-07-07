import encoding.html

fn test_escape_html() {
	assert html.escape('<>&') == '&lt;&gt;&amp;'
	assert html.escape('No_change') == 'No_change'
	assert html.escape('<b>Bold text</b>') == '&lt;b&gt;Bold&nbsp;text&lt;/b&gt;'
	assert html.escape('<img />') == '&lt;img&nbsp;/&gt;'
	assert html.escape("' onmouseover='alert(1)'") == '&apos;&nbsp;onmouseover=&apos;alert(1)&apos;'
	assert html.escape("<a href='http://www.example.com'>link</a>") == '&lt;a&nbsp;href=&apos;http://www.example.com&apos;&gt;link&lt;/a&gt;'
	assert html.escape("<script>alert('hello');</script>") == '&lt;script&gt;alert(&apos;hello&apos;);&lt;/script&gt;'
	assert html.escape_symbol('<>&') == '&lt;&gt;&amp;'
	assert html.escape_decimal('<img />') == '&#60;img&#160;/&#62;'
	assert html.escape_hex('<b>Bold text</b>') == '&#x3C;b&#x3E;Bold&#xA0;text&#x3C;/b&#x3E;'
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.escape('plain text') == 'plain&nbsp;text'
	assert html.escape('') == ''
	assert html.escape('bread & butter') == 'bread&nbsp;&amp;&nbsp;butter'
	assert html.escape('"bread" & butter') == '&quot;bread&quot;&nbsp;&amp;&nbsp;butter'
	assert html.escape('greater than >') == 'greater&nbsp;than&nbsp;&gt;'
	assert html.escape('< less than') == '&lt;&nbsp;less&nbsp;than'
	// Leave accents as-is
	assert html.escape('café') == 'café'
	assert html.escape('<p>façade</p>') == '&lt;p&gt;façade&lt;/p&gt;'
}
