import encoding.html

fn test_escape_html() {
	assert html.escape('<>&') == '&lt;&gt;&amp;'
	assert html.escape('No change') == 'No change'
	assert html.escape('<b>Bold text</b>') == '&lt;b&gt;Bold text&lt;/b&gt;'
	assert html.escape('<img />') == '&lt;img /&gt;'
	assert html.escape("' onmouseover='alert(1)'") == '&#39; onmouseover=&#39;alert(1)&#39;'
	assert html.escape("<a href='http://www.example.com'>link</a>") == '&lt;a href=&#39;http://www.example.com&#39;&gt;link&lt;/a&gt;'
	assert html.escape("<script>alert('hello');</script>") == '&lt;script&gt;alert(&#39;hello&#39;);&lt;/script&gt;'
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.escape('plain text') == 'plain text'
	assert html.escape('') == ''
	assert html.escape('bread & butter') == 'bread &amp; butter'
	assert html.escape('"bread" & butter') == '&#34;bread&#34; &amp; butter'
	assert html.escape('greater than >') == 'greater than &gt;'
	assert html.escape('< less than') == '&lt; less than'
	// Leave accents as-is
	assert html.escape('café') == 'café'
	assert html.escape('<p>façade</p>') == '&lt;p&gt;façade&lt;/p&gt;'
}

fn test_unescape_html() {
	// Test different formats
	assert html.unescape('&#39;&#x27;&apos;') == "'&#x27;&apos;"
	// Converse escape tests
	assert html.unescape('&lt;&gt;&amp;') == '<>&'
	assert html.unescape('No change') == 'No change'
	assert html.unescape('&lt;b&gt;Bold text&lt;/b&gt;') == '<b>Bold text</b>'
	assert html.unescape('&lt;img /&gt;') == '<img />'
	assert html.unescape('&#39; onmouseover=&#39;alert(1)&#39;') == "' onmouseover='alert(1)'"
	assert html.unescape('&lt;a href=&#39;http://www.example.com&#39;&gt;link&lt;/a&gt;') == "<a href='http://www.example.com'>link</a>"
	assert html.unescape('&lt;script&gt;alert(&#39;hello&#39;);&lt;/script&gt;') == "<script>alert('hello');</script>"
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.unescape('plain text') == 'plain text'
	assert html.unescape('') == ''
	assert html.unescape('bread &amp; butter') == 'bread & butter'
	assert html.unescape('&#34;bread&#34; &amp; butter') == '"bread" & butter'
	assert html.unescape('greater than &gt;') == 'greater than >'
	assert html.unescape('&lt; less than') == '< less than'
	// Leave accents as-is
	assert html.unescape('café') == 'café'
	assert html.unescape('&lt;p&gt;façade&lt;/p&gt;') == '<p>façade</p>'
}

fn test_unescape_all_html() {
	// Test different formats
	assert html.unescape('&#39;&#x27;&apos;', all: true) == "'''"
	assert html.unescape('&#10836; = &#x02a54; = &#X02A54; = &Or;', all: true) == '⩔ = ⩔ = ⩔ = ⩔'
	// Converse escape tests
	assert html.unescape('&lt;&gt;&amp;', all: true) == '<>&'
	assert html.unescape('No change', all: true) == 'No change'
	assert html.unescape('&lt;b&gt;Bold text&lt;/b&gt;', all: true) == '<b>Bold text</b>'
	assert html.unescape('&lt;img /&gt;', all: true) == '<img />'
	assert html.unescape('&#39; onmouseover=&#39;alert(1)&#39;', all: true) == "' onmouseover='alert(1)'"
	assert html.unescape('&lt;a href=&#39;http://www.example.com&#39;&gt;link&lt;/a&gt;', all: true) == "<a href='http://www.example.com'>link</a>"
	assert html.unescape('&lt;script&gt;alert(&#39;hello&#39;);&lt;/script&gt;', all: true) == "<script>alert('hello');</script>"
	// Cases obtained from:
	// https://github.com/apache/commons-lang/blob/master/src/test/java/org/apache/commons/lang3/StringEscapeUtilsTest.java
	assert html.unescape('plain text', all: true) == 'plain text'
	assert html.unescape('', all: true) == ''
	assert html.unescape('bread &amp; butter', all: true) == 'bread & butter'
	assert html.unescape('&#34;bread&#34; &amp; butter', all: true) == '"bread" & butter'
	assert html.unescape('greater than &gt;', all: true) == 'greater than >'
	assert html.unescape('&lt; less than', all: true) == '< less than'
	// Leave accents as-is
	assert html.unescape('café', all: true) == 'café'
	assert html.unescape('&lt;p&gt;façade&lt;/p&gt;', all: true) == '<p>façade</p>'
}
