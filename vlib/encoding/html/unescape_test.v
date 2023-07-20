import encoding.html

fn test_unescape_html() {
	assert html.unescape('&lt;&gt;&amp;') == '<>&'
	assert html.unescape('No change') == 'No change'
	assert html.unescape('&lt;b&gt;Bold text&lt;/b&gt;') == '<b>Bold text</b>'
	assert html.unescape('&lt;img /&gt;') == '<img />'
	assert html.unescape('&apos; onmouseover=&apos;alert(1)&apos;') == "' onmouseover='alert(1)'"
	assert html.unescape('&lt;a href=&apos;http://www.example.com&apos;&gt;link&lt;/a&gt;') == "<a href='http://www.example.com'>link</a>"
	assert html.unescape('&lt;script&gt;alert(&apos;hello&apos;);&lt;/script&gt;') == "<script>alert('hello');</script>"
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.unescape('plain text') == 'plain text'
	assert html.unescape('') == ''
	assert html.unescape('bread &amp; butter') == 'bread & butter'
	assert html.unescape('&#34;bread&#34; &amp; butter') == '"bread" & butter'
	assert html.unescape('greater than &gt;') == 'greater than >'
	assert html.unescape('&lt; less than') == '< less than'
	// Leave accents as-is
	assert html.escape('café') == 'café'
	assert html.escape('<p>façade</p>') == '&lt;p&gt;façade&lt;/p&gt;'
}
