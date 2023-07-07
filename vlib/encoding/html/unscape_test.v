import encoding.html

fn test_unescape_html() {
	assert html.unescape('&lt;&gt;&amp;') == '<>&'
	assert html.unescape('No&nbsp;change') == 'No change'
	assert html.unescape('&lt;b&gt;Bold&nbsp;text&lt;/b&gt;') == '<b>Bold text</b>'
	assert html.unescape('&lt;img&nbsp;/&gt;') == '<img />'
	assert html.unescape('&apos;&nbsp;onmouseover=&apos;alert(1)&apos;') == "' onmouseover='alert(1)'"
	assert html.unescape('&lt;a&nbsp;href=&apos;http://www.example.com&apos;&gt;link&lt;/a&gt;') == "<a href='http://www.example.com'>link</a>"
	assert html.unescape('&lt;script&gt;alert(&apos;hello&apos;);&lt;/script&gt;') == "<script>alert('hello');</script>"
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.unescape('plain&nbsp;text') == 'plain text'
	assert html.unescape('') == ''
	assert html.unescape('bread&nbsp;&amp;&nbsp;butter') == 'bread & butter'
	assert html.unescape('&quot;bread&quot;&nbsp;&amp;&nbsp;butter') == '"bread" & butter'
	assert html.unescape('greater&nbsp;than&nbsp;&gt;') == 'greater than >'
	assert html.unescape('&lt;&nbsp;less&nbsp;than') == '< less than'
	// Leave accents as-is
	assert html.escape('café') == 'café'
	assert html.escape('<p>façade</p>') == '&lt;p&gt;façade&lt;/p&gt;'
}
